(import peg)
(import os)

(defn -xml-space-byte?
  "Return true when byte is XML whitespace."
  [(Integer byte)]
  (or (= byte 0x20)
      (= byte 0x09)
      (= byte 0x0A)
      (= byte 0x0D)))

(defn -xml-blank-text?
  "Return true when text contains only XML whitespace."
  [text]
  (let [i 0
        n (len text)
        blank true]
    (while (and blank (< i n))
      (if (-xml-space-byte? (get text i))
        (incq i)
        (setq blank false)))
    blank))

(def -xml-entity-lt-peg (peg/compile "&lt;"))
(def -xml-entity-gt-peg (peg/compile "&gt;"))
(def -xml-entity-quot-peg (peg/compile "&quot;"))
(def -xml-entity-apos-peg (peg/compile "&apos;"))
(def -xml-entity-amp-peg (peg/compile "&amp;"))

(defn -xml-has-entity-marker?
  "Return true when text contains '&'."
  [text]
  (let [i 0
        n (len text)
        found false]
    (while (and (not found) (< i n))
      (if (= (get text i) 0x26)
        (setq found true)
        (incq i)))
    found))

(defn -xml-decode-basic-entities
  "Decode the five predefined XML entities."
  [text]
  (if (not (-xml-has-entity-marker? text))
    text
    (let [decoded text]
      (setq decoded (peg/replace -xml-entity-lt-peg "<" decoded))
      (setq decoded (peg/replace -xml-entity-gt-peg ">" decoded))
      (setq decoded (peg/replace -xml-entity-quot-peg "\"" decoded))
      (setq decoded (peg/replace -xml-entity-apos-peg "'" decoded))
      (setq decoded (peg/replace -xml-entity-amp-peg "&" decoded))
      decoded)))

(defn -xml-make-attribute
  [name value]
  [(keyword name) value])

(defn -xml-attrs->map
  [& entries]
  (let [attrs {}]
    (for [entry entries]
      (let [[k v] entry]
        (put attrs k v)))
    attrs))

(defn -xml-text-node
  [text]
  (let [decoded (-xml-decode-basic-entities text)]
    (if (-xml-blank-text? decoded)
      nil
      decoded)))

(defn -xml-compact-children
  [children]
  (let [out nil]
    (for [child children]
      (if (some? child)
        (setq out (cons child out))))
    (Vector (nreverse out))))

(defn -xml-make-empty-element
  [name attrs]
  [(keyword name) attrs])

(defn -xml-make-full-element
  [open-name attrs children close-name]
  (if (not= open-name close-name)
    (raise 'xml
           "mismatched closing tag: expected </%s>, got </%s>"
           open-name
           close-name))
  (catv [(keyword open-name) attrs]
        (-xml-compact-children children)))

(defn -xml-make-pi-node
  [name attrs]
  [(keyword (str "?" name)) attrs])

(defn -xml-make-xml-decl
  [attrs]
  [:?xml attrs])

(defn -xml-document-no-decl
  [root]
  root)

(defn -xml-document-with-decl
  [decl root]
  (catv decl [root]))

(def -xml-parser
  (peg/compile
   {:main '(and :document (not 1))
    :document '(or :document-with-decl
                   :document-no-decl)
    :document-with-decl `(cmt (and :misc*
                                   :xml-decl
                                   :misc*
                                   :element
                                   :misc*)
                              ,-xml-document-with-decl)
    :document-no-decl `(cmt (and :misc*
                                 :element
                                 :misc*)
                            ,-xml-document-no-decl)
    :misc* '(any :misc)
    :misc '(or :s+ :comment)
    :comment '(drop (and "<!--" (to "-->") "-->"))
    :xml-decl `(cmt (and "<?xml"
                         :attrs
                         :s*
                         "?>")
                    ,-xml-make-xml-decl)
    :element '(or :empty-element
                  :full-element)
    :empty-element `(cmt (and "<"
                              (capture :name)
                              :attrs
                              :s*
                              "/>")
                         ,-xml-make-empty-element)
    :full-element `(cmt (and "<"
                             (capture :name)
                             :attrs
                             :s*
                             ">"
                             (accumulate (any :content-item))
                             "</"
                             (capture :name)
                             :s*
                             ">")
                        ,-xml-make-full-element)
    :content-item '(or :comment
                       :element
                       :pi-node
                       :text-node)
    :pi-node `(cmt (and "<?"
                        (capture :name)
                        :attrs
                        :s*
                        "?>")
                   ,-xml-make-pi-node)
    :text-node `(cmt (capture (some (and (not "<") 1)))
                     ,-xml-text-node)
    :attrs `(cmt (any :attribute) ,-xml-attrs->map)
    :attribute `(cmt (and :s+
                          (capture :name)
                          :s*
                          "="
                          :s*
                          :attr-value)
                     ,-xml-make-attribute)
    :attr-value `(or (cmt (and "\""
                               (capture (any (and (not "\"") 1)))
                               "\"")
                          ,-xml-decode-basic-entities)
                     (cmt (and "'"
                               (capture (any (and (not "'") 1)))
                               "'")
                          ,-xml-decode-basic-entities))
    :name '(and :name-start
                (any :name-char))
    :name-start '(or (range "az" "AZ")
                     (set "_:"))
    :name-char '(or :name-start
                    :d
                    (set "-."))}))

(defn parse
  "Parse XML from a String or Stream into Hiccup vectors."
  [input]
  (let [captures (peg/match -xml-parser input)]
    (if (nil? captures)
      (raise 'xml "invalid XML input")
      (get captures 0))))

(defn parse-file
  "Parse XML file from path into Hiccup vectors."
  [(String path)]
  (let [f (os/open path os/O_RDONLY)]
    (try
      (parse (stream f))
      (%finally (os/close f)))))
