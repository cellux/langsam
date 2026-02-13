(import xml)
(import os)

(assert (= (xml/parse "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root attr=\"ok\"><child>value</child><empty/></root>")
           [:?xml {:version "1.0" :encoding "UTF-8"}
            [:root {:attr "ok"}
             [:child {} "value"]
             [:empty {}]]]))

(assert (= (xml/parse "<root><!-- ignore --><a> x </a><b>\n  </b>tail</root>")
           [:root {}
            [:a {} " x "]
            [:b {}]
            "tail"]))

(assert (= (xml/parse "<r>&lt;&amp;&gt;&quot;&apos;&#960;</r>")
           [:r {} "<&>\"'&#960;"]))

(let [path "/tmp/langsam-xml-parse-file-test.xml"
      source "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<x86reference version=\"1.14\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"x86reference.xsd\"><one-byte/></x86reference>"]
  (try
    (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
      (os/write f source)
      (os/close f))
    (let [parsed (xml/parse-file path)
          decl-attrs (get parsed 1)
          root (get parsed 2)
          root-attrs (get root 1)
          first-section (get root 2)]
      (assert (= (get parsed 0) :?xml))
      (assert (= (get decl-attrs :version) "1.0"))
      (assert (= (get decl-attrs :encoding) "UTF-8"))
      (assert (= (get root 0) :x86reference))
      (assert (= (get root-attrs :version) "1.14"))
      (assert (= (get root-attrs (keyword "xmlns:xsi"))
                 "http://www.w3.org/2001/XMLSchema-instance"))
      (assert (= (get root-attrs (keyword "xsi:noNamespaceSchemaLocation"))
                 "x86reference.xsd"))
      (assert (= first-section [:one-byte {}])))
    (%finally (catch [_ nil] (os/unlink path)))))
