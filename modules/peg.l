(defn make-default-aliases
  "Build Janet-like default PEG aliases."
  []
  (let [base {:d '(range "09")
              :D '(not :d)
              :a '(range "az" "AZ")
              :A '(not :a)
              :h '(or :d (range "af" "AF"))
              :H '(not :h)
              :s '(set " \t\r\n\0\v\f")
              :S '(not :s)
              :w '(or :a :d)
              :W '(not :w)}]
    (for [k (keys base)]
      (let [name (name k)
            plus (keyword (str name "+"))
            star (keyword (str name "*"))]
        (put base plus `(some ,k))
        (put base star `(any ,k))))
    base))

(def peg-default-aliases (make-default-aliases))

(defn compiled-peg?
  "Return true when x is a compiled PEG object."
  [x]
  (and (map? x)
       (= (:kind x) :compiled-peg)))

(defn peg-root
  "Choose initial root pattern for compile skeleton."
  [peg-src]
  (if (map? peg-src)
    (if (contains? peg-src :main)
      (get peg-src :main)
      (raise 'peg "grammar map requires :main rule"))
    peg-src))

(def peg-op-aliases
  {'+ 'or
   '* 'and
   '> 'look
   '? 'opt
   '<- 'capture
   '/ 'replace
   '$ 'position
   '-> 'backref
   '% 'accumulate})

(defn peg-canonical-op
  "Normalize syntactic PEG operator aliases."
  [op]
  (if (contains? peg-op-aliases op)
    (get peg-op-aliases op)
    op))

(defn peg-validate-reference
  "Ensure keyword/symbol ref resolves in rules/default aliases."
  [compiled ref]
  (if (nil? (peg-resolve-ref compiled ref))
    (raise 'peg "unknown PEG reference: %s" ref)))

(defn peg-validate-range-entries
  "Validate compile-time shape for (range ...)."
  [args]
  (for [entry args]
    (if (not (string? entry))
      (raise 'peg "range expects String entries, got %s" (type entry)))
    (if (not (= (len entry) 2))
      (raise 'peg "range entries must have length 2, got %s" (len entry)))))

(defn peg-push-patterns
  "Push all args onto stack in iteration order."
  [stack args]
  (for [arg args]
    (setq stack (cons arg stack)))
  stack)

(defn peg-validate-form-push-first
  [ctx op-name]
  (if (= (:argc ctx) 1)
    (cons (get (:argv ctx) 0) (:stack ctx))
    (raise 'peg "%s expects 1 arg, got %s" op-name (:argc ctx))))

(defn peg-validate-form-push-first-1or2
  [ctx op-name]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 1) (= argc 2))
      (cons (get argv 0) stack)
      (raise 'peg "%s expects 1 or 2 args, got %s" op-name argc))))

(defn peg-validate-form-push-first-2or3
  [ctx op-name]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 2) (= argc 3))
      (cons (get argv 0) stack)
      (raise 'peg "%s expects 2 or 3 args, got %s" op-name argc))))

(defn peg-validate-form-push-first-two
  [ctx op-name]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 2)
      (do
        (setq stack (cons (get argv 0) stack))
        (setq stack (cons (get argv 1) stack))
        stack)
      (raise 'peg "%s expects 2 args, got %s" op-name argc))))

(defn peg-validate-form-arity-0or1
  [ctx op-name]
  (let [{:keys [argc stack]} ctx]
    (if (or (= argc 0) (= argc 1))
      stack
      (raise 'peg "%s expects 0 or 1 args, got %s" op-name argc))))

(defn peg-validate-form-arity-1or2
  [ctx op-name]
  (let [{:keys [argc stack]} ctx]
    (if (or (= argc 1) (= argc 2))
      stack
      (raise 'peg "%s expects 1 or 2 args, got %s" op-name argc))))

(defn peg-validate-form-validate-width-1or2
  [ctx op-name]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 1) (= argc 2))
      (let [width (get argv 0)]
        (if (not (integer? width))
          (raise 'peg "%s width should be Integer, got %s" op-name (type width)))
        (if (<= width 0)
          (raise 'peg "integer width should be positive, got %s" width))
        stack)
      (raise 'peg "%s expects 1 or 2 args, got %s" op-name argc))))

(defn peg-validate-form-dispatch
  [ctx]
  (let [{:keys [raw-op op]} ctx]
    (if (integer? raw-op)
      :numeric-repeat
      op)))

(defmulti peg-validate-form-op
  "Validate one PEG list-form operator and return updated stack."
  peg-validate-form-dispatch)

(defmethod peg-validate-form-op 'or
  [ctx]
  (peg-push-patterns (:stack ctx) (:args ctx)))

(defmethod peg-validate-form-op 'and
  [ctx]
  (peg-push-patterns (:stack ctx) (:args ctx)))

(defmethod peg-validate-form-op 'any
  [ctx]
  (peg-validate-form-push-first ctx "any"))

(defmethod peg-validate-form-op 'some
  [ctx]
  (peg-validate-form-push-first ctx "some"))

(defmethod peg-validate-form-op 'repeat
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 2)
      (let [n (get argv 0)]
        (if (not (integer? n))
          (raise 'peg "repeat count should be Integer, got %s" (type n)))
        (if (< n 0)
          (raise 'peg "repeat count must be non-negative, got %s" n))
        (cons (get argv 1) stack))
      (raise 'peg "repeat expects 2 args, got %s" argc))))

(defmethod peg-validate-form-op 'at-least
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 2)
      (let [n (get argv 0)]
        (if (not (integer? n))
          (raise 'peg "at-least count should be Integer, got %s" (type n)))
        (if (< n 0)
          (raise 'peg "at-least count must be non-negative, got %s" n))
        (cons (get argv 1) stack))
      (raise 'peg "at-least expects 2 args, got %s" argc))))

(defmethod peg-validate-form-op 'at-most
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 2)
      (let [n (get argv 0)]
        (if (not (integer? n))
          (raise 'peg "at-most count should be Integer, got %s" (type n)))
        (if (< n 0)
          (raise 'peg "at-most count must be non-negative, got %s" n))
        (cons (get argv 1) stack))
      (raise 'peg "at-most expects 2 args, got %s" argc))))

(defmethod peg-validate-form-op 'between
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 3)
      (let [nmin (get argv 0)
            nmax (get argv 1)]
        (if (or (not (integer? nmin)) (not (integer? nmax)))
          (raise 'peg "between counts should be Integer, got %s %s"
                 (type nmin) (type nmax)))
        (if (< nmin 0)
          (raise 'peg "between min must be non-negative, got %s" nmin))
        (if (< nmax 0)
          (raise 'peg "between max must be non-negative, got %s" nmax))
        (if (> nmin nmax)
          (raise 'peg "between expects min <= max, got %s > %s" nmin nmax))
        (cons (get argv 2) stack))
      (raise 'peg "between expects 3 args, got %s" argc))))

(defmethod peg-validate-form-op 'if
  [ctx]
  (peg-validate-form-push-first-two ctx "if"))

(defmethod peg-validate-form-op 'if-not
  [ctx]
  (peg-validate-form-push-first-two ctx "if-not"))

(defmethod peg-validate-form-op 'not
  [ctx]
  (peg-validate-form-push-first ctx "not"))

(defmethod peg-validate-form-op 'look
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 1)
      (cons (get argv 0) stack)
      (if (= argc 2)
        (let [offset (get argv 0)]
          (if (not (integer? offset))
            (raise 'peg "look offset should be Integer, got %s" (type offset)))
          (cons (get argv 1) stack))
        (raise 'peg "look expects 1 or 2 args, got %s" argc)))))

(defmethod peg-validate-form-op 'to
  [ctx]
  (peg-validate-form-push-first ctx "to"))

(defmethod peg-validate-form-op 'thru
  [ctx]
  (peg-validate-form-push-first ctx "thru"))

(defmethod peg-validate-form-op 'sub
  [ctx]
  (peg-validate-form-push-first-two ctx "sub"))

(defmethod peg-validate-form-op 'split
  [ctx]
  (peg-validate-form-push-first-two ctx "split"))

(defmethod peg-validate-form-op 'opt
  [ctx]
  (peg-validate-form-push-first ctx "opt"))

(defmethod peg-validate-form-op 'capture
  [ctx]
  (peg-validate-form-push-first-1or2 ctx "capture"))

(defmethod peg-validate-form-op 'quote
  [ctx]
  (peg-validate-form-push-first-1or2 ctx "quote"))

(defmethod peg-validate-form-op 'replace
  [ctx]
  (peg-validate-form-push-first-2or3 ctx "replace"))

(defmethod peg-validate-form-op 'group
  [ctx]
  (peg-validate-form-push-first-1or2 ctx "group"))

(defmethod peg-validate-form-op 'accumulate
  [ctx]
  (peg-validate-form-push-first-1or2 ctx "accumulate"))

(defmethod peg-validate-form-op 'cmt
  [ctx]
  (peg-validate-form-push-first-2or3 ctx "cmt"))

(defmethod peg-validate-form-op 'drop
  [ctx]
  (peg-validate-form-push-first ctx "drop"))

(defmethod peg-validate-form-op 'unref
  [ctx]
  (peg-validate-form-push-first-1or2 ctx "unref"))

(defmethod peg-validate-form-op 'only-tags
  [ctx]
  (peg-validate-form-push-first ctx "only-tags"))

(defmethod peg-validate-form-op 'nth
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 2) (= argc 3))
      (let [index (get argv 0)]
        (if (not (integer? index))
          (raise 'peg "nth index should be Integer, got %s" (type index)))
        (cons (get argv 1) stack))
      (raise 'peg "nth expects 2 or 3 args, got %s" argc))))

(defmethod peg-validate-form-op 'number
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 1) (= argc 2) (= argc 3))
      (do
        (if (> argc 1)
          (let [base (get argv 1)]
            (if (and (some? base) (not (integer? base)))
              (raise 'peg "number base should be Integer, got %s" (type base)))))
        (cons (get argv 0) stack))
      (raise 'peg "number expects 1 to 3 args, got %s" argc))))

(defmethod peg-validate-form-op 'lenprefix
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 2)
      (let [width (get argv 0)]
        (if (not (integer? width))
          (raise 'peg "lenprefix width should be Integer, got %s" (type width)))
        (if (<= width 0)
          (raise 'peg "integer width should be positive, got %s" width))
        (cons (get argv 1) stack))
      (raise 'peg "lenprefix expects 2 args, got %s" argc))))

(defmethod peg-validate-form-op 'error
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 0) (= argc 1))
      (if (= argc 1)
        (cons (get argv 0) stack)
        stack)
      (raise 'peg "error expects 0 or 1 args, got %s" argc))))

(defmethod peg-validate-form-op 'constant
  [ctx]
  (peg-validate-form-arity-1or2 ctx "constant"))

(defmethod peg-validate-form-op 'argument
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (or (= argc 1) (= argc 2))
      (let [index (get argv 0)]
        (if (not (integer? index))
          (raise 'peg "argument index should be Integer, got %s" (type index)))
        (if (< index 0)
          (raise 'peg "argument index should be non-negative, got %s" index))
        stack)
      (raise 'peg "argument expects 1 or 2 args, got %s" argc))))

(defmethod peg-validate-form-op 'uint
  [ctx]
  (peg-validate-form-validate-width-1or2 ctx "uint"))

(defmethod peg-validate-form-op 'uint-be
  [ctx]
  (peg-validate-form-validate-width-1or2 ctx "uint-be"))

(defmethod peg-validate-form-op 'int
  [ctx]
  (peg-validate-form-validate-width-1or2 ctx "int"))

(defmethod peg-validate-form-op 'int-be
  [ctx]
  (peg-validate-form-validate-width-1or2 ctx "int-be"))

(defmethod peg-validate-form-op 'position
  [ctx]
  (peg-validate-form-arity-0or1 ctx "position"))

(defmethod peg-validate-form-op 'line
  [ctx]
  (peg-validate-form-arity-0or1 ctx "line"))

(defmethod peg-validate-form-op 'column
  [ctx]
  (peg-validate-form-arity-0or1 ctx "column"))

(defmethod peg-validate-form-op 'backref
  [ctx]
  (peg-validate-form-arity-1or2 ctx "backref"))

(defmethod peg-validate-form-op 'backmatch
  [ctx]
  (peg-validate-form-arity-0or1 ctx "backmatch"))

(defmethod peg-validate-form-op 'range
  [ctx]
  (peg-validate-range-entries (:args ctx))
  (:stack ctx))

(defmethod peg-validate-form-op 'set
  [ctx]
  (let [{:keys [argc argv stack]} ctx]
    (if (= argc 1)
      (let [charset (get argv 0)]
        (if (not (string? charset))
          (raise 'peg "set expects String charset, got %s" (type charset)))
        stack)
      (raise 'peg "set expects 1 arg, got %s" argc))))

(defmethod peg-validate-form-op :numeric-repeat
  [ctx]
  (let [{:keys [raw-op argc argv stack]} ctx]
    (if (= argc 1)
      (do
        (if (< raw-op 0)
          (raise 'peg "repeat count must be non-negative, got %s" raw-op))
        (cons (get argv 0) stack))
      (raise 'peg "numeric repeat expects 1 arg, got %s" argc))))

(defmethod peg-validate-form-op :default
  [ctx]
  (raise 'peg "unsupported PEG operator: %s" (:raw-op ctx)))

(defn peg-validate-form
  "Validate one list-form PEG pattern and return updated stack."
  [compiled pattern stack]
  (peg-validate-form-op {:compiled compiled
                         :pattern pattern
                         :stack stack
                         :raw-op (car pattern)
                         :op (peg-canonical-op (car pattern))
                         :args (cdr pattern)
                         :argc (len (cdr pattern))
                         :argv (Vector (cdr pattern))}))

(defmulti peg-validate-pattern
  "Validate one PEG pattern and return updated validation stack."
  (fn [pattern _compiled _stack]
    (type pattern)))

(defmethod peg-validate-pattern String
  [_pattern _compiled stack]
  stack)

(defmethod peg-validate-pattern Integer
  [_pattern _compiled stack]
  stack)

(defmethod peg-validate-pattern Boolean
  [_pattern _compiled stack]
  stack)

(defmethod peg-validate-pattern Keyword
  [pattern compiled stack]
  (peg-validate-reference compiled pattern)
  stack)

(defmethod peg-validate-pattern Symbol
  [pattern compiled stack]
  (peg-validate-reference compiled pattern)
  stack)

(defmethod peg-validate-pattern Cons
  [pattern compiled stack]
  (peg-validate-form compiled pattern stack))

(defmethod peg-validate-pattern :default
  [pattern _compiled _stack]
  (raise 'peg "unsupported PEG pattern type: %s" (type pattern)))

(defn peg-validate-compiled
  "Validate PEG arity/type/ref checks iteratively at compile time."
  [compiled]
  (let [stack (cons (:root compiled) nil)
        rules (:rules compiled)]
    (if (map? rules)
      (for [[_ rule] rules]
        (setq stack (cons rule stack))))
    (while stack
      (let [pattern (car stack)]
        (setq stack (cdr stack))
        (setq stack (peg-validate-pattern pattern compiled stack))))
    compiled))

(defn peg-ir-op-child-arg-indexes
  "Return Vector of argument indexes that are child patterns for op."
  [raw-op op (Integer argc)]
  (if (integer? raw-op)
    [0]
    (case op
      'or (let [idxs nil
                i argc]
            (while (> i 0)
              (decq i)
              (setq idxs (cons i idxs)))
            (Vector idxs))
      'and (let [idxs nil
                 i argc]
             (while (> i 0)
               (decq i)
               (setq idxs (cons i idxs)))
             (Vector idxs))
      'any [0]
      'some [0]
      'repeat [1]
      'at-least [1]
      'at-most [1]
      'between [2]
      'if [0 1]
      'if-not [0 1]
      'not [0]
      'look (if (= argc 2) [1] [0])
      'sub [0 1]
      'opt [0]
      'capture [0]
      'quote [0]
      'replace [0]
      'group [0]
      'accumulate [0]
      'cmt [0]
      'drop [0]
      'unref [0]
      'only-tags [0]
      'nth [1]
      'number [0]
      'lenprefix [1]
      'error (if (= argc 1) [0] [])
      [])))

(defn peg-ir-form1 [op a]
  (cons op (cons a nil)))

(defn peg-ir-form2 [op a b]
  (cons op (cons a (cons b nil))))

(defn peg-ir-lower-op-form
  "Lower op forms that are represented as canonical composites in v1."
  [op argv argc]
  (case op
    'to (if (= argc 1)
          (let [target (get argv 0)]
            (peg-ir-form2 'and
                          (peg-ir-form1 'any
                                        (peg-ir-form2 'and
                                                      (peg-ir-form1 'not target)
                                                      true))
                          (peg-ir-form1 'look target)))
          nil)
    'thru (if (= argc 1)
            (let [target (get argv 0)]
              (peg-ir-form2 'and
                            (peg-ir-form1 'to target)
                            target))
            nil)
    'split (if (= argc 2)
             (let [sep (get argv 0)
                   child (get argv 1)]
               (peg-ir-form1 'opt
                             (peg-ir-form2 'and
                                           child
                                           (peg-ir-form1 'any
                                                         (peg-ir-form2 'and sep child)))))
             nil)
    nil))

(defn peg-ir-add-node!
  [ctx node]
  (let [id (:next-id ctx)]
    (put (:nodes-map ctx) id node)
    (put ctx :next-id (+ id 1))
    id))

(defn peg-ir-new-slot!
  [ctx]
  (let [slot (:next-slot ctx)]
    (put ctx :next-slot (+ slot 1))
    slot))

(defn peg-ir-build-node
  "Build one normalized IR node from canonical op/raw-op plus child ids."
  [raw-op op args argc argv child-ids]
  (if (integer? raw-op)
    {:op :repeat
     :count raw-op
     :child (get child-ids 0)}
    (case op
      'or {:op :or
           :children child-ids}
      'and {:op :and
            :children child-ids}
      'any {:op :any
            :child (get child-ids 0)}
      'some {:op :some
             :child (get child-ids 0)}
      'repeat {:op :repeat
               :count (get argv 0)
               :child (get child-ids 0)}
      'at-least {:op :at-least
                 :count (get argv 0)
                 :child (get child-ids 0)}
      'at-most {:op :at-most
                :count (get argv 0)
                :child (get child-ids 0)}
      'between {:op :between
                :min (get argv 0)
                :max (get argv 1)
                :child (get child-ids 0)}
      'if {:op :if
           :cond (get child-ids 0)
           :child (get child-ids 1)
           :neg false}
      'if-not {:op :if
               :cond (get child-ids 0)
               :child (get child-ids 1)
               :neg true}
      'not {:op :not
            :child (get child-ids 0)}
      'look {:op :look
             :child (get child-ids 0)
             :offset (if (= argc 2) (get argv 0) 0)}
      'sub {:op :sub
            :window (get child-ids 0)
            :child (get child-ids 1)}
      'opt {:op :opt
            :child (get child-ids 0)}
      'capture {:op :capture
                :child (get child-ids 0)
                :tag (if (= argc 2) (get argv 1) nil)}
      'quote {:op :capture
              :child (get child-ids 0)
              :tag (if (= argc 2) (get argv 1) nil)}
      'replace {:op :replace-cap
                :child (get child-ids 0)
                :subst (get argv 1)
                :tag (if (= argc 3) (get argv 2) nil)}
      'group {:op :group
              :child (get child-ids 0)
              :tag (if (= argc 2) (get argv 1) nil)}
      'accumulate {:op :accumulate
                   :child (get child-ids 0)
                   :tag (if (= argc 2) (get argv 1) nil)}
      'cmt {:op :cmt
            :child (get child-ids 0)
            :f (get argv 1)
            :tag (if (= argc 3) (get argv 2) nil)}
      'drop {:op :drop
             :child (get child-ids 0)}
      'unref {:op :unref
              :child (get child-ids 0)
              :tag (if (= argc 2) (get argv 1) nil)}
      'only-tags {:op :only-tags
                  :child (get child-ids 0)}
      'nth {:op :nth
            :child (get child-ids 0)
            :index (get argv 0)
            :tag (if (= argc 3) (get argv 2) nil)}
      'number {:op :number
               :child (get child-ids 0)
               :base (if (> argc 1) (get argv 1) nil)
               :tag (if (= argc 3) (get argv 2) nil)}
      'lenprefix {:op :lenprefix
                  :width (get argv 0)
                  :child (get child-ids 0)}
      'error {:op :error
              :child (if (= argc 1) (get child-ids 0) nil)}
      'constant {:op :constant
                 :value (get argv 0)
                 :tag (if (= argc 2) (get argv 1) nil)}
      'argument {:op :argument
                 :index (get argv 0)
                 :tag (if (= argc 2) (get argv 1) nil)}
      'position {:op :position
                 :tag (if (= argc 1) (get argv 0) nil)}
      'line {:op :line
             :tag (if (= argc 1) (get argv 0) nil)}
      'column {:op :column
               :tag (if (= argc 1) (get argv 0) nil)}
      'backref {:op :backref
                :src-tag (get argv 0)
                :out-tag (if (= argc 2) (get argv 1) nil)}
      'backmatch {:op :backmatch
                  :src-tag (if (= argc 1) (get argv 0) nil)}
      'uint {:op :uint
             :width (get argv 0)
             :be false
             :signed false
             :tag (if (= argc 2) (get argv 1) nil)}
      'uint-be {:op :uint
                :width (get argv 0)
                :be true
                :signed false
                :tag (if (= argc 2) (get argv 1) nil)}
      'int {:op :uint
            :width (get argv 0)
            :be false
            :signed true
            :tag (if (= argc 2) (get argv 1) nil)}
      'int-be {:op :uint
               :width (get argv 0)
               :be true
               :signed true
               :tag (if (= argc 2) (get argv 1) nil)}
      'range {:op :range
              :ranges (Vector args)}
      'set {:op :set
            :charset (get argv 0)}
      (raise 'peg "unsupported PEG operator for IR build: %s" raw-op))))

(defn peg-ir-compile-pattern!
  "Compile one raw pattern into normalized IR and return root node id."
  [ctx pattern]
  (let [slots (:slots ctx)
        root-slot (peg-ir-new-slot! ctx)
        stack (cons {:kind :visit
                     :slot root-slot
                     :pattern pattern}
                    nil)]
    (while stack
      (let [task (car stack)]
        (setq stack (cdr stack))
        (if (= (:kind task) :visit)
          (let [cur (:pattern task)
                slot (:slot task)]
            (cond
             (string? cur)
             (put slots slot (peg-ir-add-node! ctx {:op :lit :text cur}))
             (integer? cur)
             (put slots slot (peg-ir-add-node! ctx {:op :bytes :count cur}))
             (boolean? cur)
             (put slots slot (peg-ir-add-node! ctx {:op :bool :value cur}))
             (keyword? cur)
             (let [id (peg-ir-add-node! ctx {:op :ref
                                             :name cur
                                             :target nil})]
               (put slots slot id)
               (put ctx :unresolved (cons {:id id :name cur} (:unresolved ctx))))
             (symbol? cur)
             (let [id (peg-ir-add-node! ctx {:op :ref
                                             :name cur
                                             :target nil})]
               (put slots slot id)
               (put ctx :unresolved (cons {:id id :name cur} (:unresolved ctx))))
             (cons? cur)
             (let [raw-op (car cur)
                   op (peg-canonical-op raw-op)
                   args (cdr cur)
                   argc (len args)
                   argv (Vector args)
                   lowered (peg-ir-lower-op-form op argv argc)]
               (if lowered
                 (setq stack (cons {:kind :visit
                                    :slot slot
                                    :pattern lowered}
                                   stack))
                 (let [child-indexes (peg-ir-op-child-arg-indexes raw-op op argc)
                       child-slots nil
                       i (- (len child-indexes) 1)]
                   (while (>= i 0)
                     (setq child-slots (cons (peg-ir-new-slot! ctx) child-slots))
                     (decq i))
                   (let [child-slot-vec (Vector child-slots)]
                     (setq stack (cons {:kind :emit
                                        :slot slot
                                        :raw-op raw-op
                                        :op op
                                        :args args
                                        :argc argc
                                        :argv argv
                                        :child-slots child-slot-vec}
                                       stack))
                     (setq i (- (len child-indexes) 1))
                     (while (>= i 0)
                       (setq stack (cons {:kind :visit
                                          :slot (get child-slot-vec i)
                                          :pattern (get argv (get child-indexes i))}
                                         stack))
                       (decq i))))))
             (raise 'peg "unsupported PEG pattern type for IR build: %s" (type cur))))
          (let [child-slots (:child-slots task)
                child-ids nil
                i (- (len child-slots) 1)]
            (while (>= i 0)
              (setq child-ids (cons (get slots (get child-slots i)) child-ids))
              (decq i))
            (let [node (peg-ir-build-node (:raw-op task)
                                          (:op task)
                                          (:args task)
                                          (:argc task)
                                          (:argv task)
                                          (Vector child-ids))]
              (put slots (:slot task) (peg-ir-add-node! ctx node))))))
    (get slots root-slot))))

(defn peg-ir-node-id-valid?
  [id (Integer node-count)]
  (and (integer? id)
       (<= 0 id)
       (< id node-count)))

(defn peg-ir-assert-node-id
  [id (Integer node-count) label]
  (if (not (peg-ir-node-id-valid? id node-count))
    (raise 'peg "invalid IR %s node id: %s" label id)))

(defn peg-ir-assert-node-field
  [node field]
  (if (not (contains? node field))
    (raise 'peg "IR node %s missing required field %s"
           (:op node)
           field)))

(defn peg-ir-validate-node
  "Validate required fields and child ids for one IR node."
  [node (Integer node-count)]
  (if (not (contains? node :op))
    (raise 'peg "IR node missing :op field"))
  (let [op (:op node)]
    (case op
      :lit (peg-ir-assert-node-field node :text)
      :bytes (peg-ir-assert-node-field node :count)
      :bool (peg-ir-assert-node-field node :value)
      :set (peg-ir-assert-node-field node :charset)
      :range (peg-ir-assert-node-field node :ranges)
      :or (do
            (peg-ir-assert-node-field node :children)
            (for [child (:children node)]
              (peg-ir-assert-node-id child node-count "child")))
      :and (do
             (peg-ir-assert-node-field node :children)
             (for [child (:children node)]
               (peg-ir-assert-node-id child node-count "child")))
      :any (do
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :some (do
              (peg-ir-assert-node-field node :child)
              (peg-ir-assert-node-id (:child node) node-count "child"))
      :repeat (do
                (peg-ir-assert-node-field node :count)
                (peg-ir-assert-node-field node :child)
                (peg-ir-assert-node-id (:child node) node-count "child"))
      :at-least (do
                  (peg-ir-assert-node-field node :count)
                  (peg-ir-assert-node-field node :child)
                  (peg-ir-assert-node-id (:child node) node-count "child"))
      :at-most (do
                 (peg-ir-assert-node-field node :count)
                 (peg-ir-assert-node-field node :child)
                 (peg-ir-assert-node-id (:child node) node-count "child"))
      :between (do
                 (peg-ir-assert-node-field node :min)
                 (peg-ir-assert-node-field node :max)
                 (peg-ir-assert-node-field node :child)
                 (peg-ir-assert-node-id (:child node) node-count "child"))
      :if (do
            (peg-ir-assert-node-field node :cond)
            (peg-ir-assert-node-field node :child)
            (peg-ir-assert-node-field node :neg)
            (peg-ir-assert-node-id (:cond node) node-count "cond")
            (peg-ir-assert-node-id (:child node) node-count "child"))
      :not (do
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :look (do
              (peg-ir-assert-node-field node :child)
              (peg-ir-assert-node-field node :offset)
              (peg-ir-assert-node-id (:child node) node-count "child"))
      :opt (do
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :sub (do
             (peg-ir-assert-node-field node :window)
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-id (:window node) node-count "window")
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :ref (do
             (peg-ir-assert-node-field node :name)
             (peg-ir-assert-node-field node :target)
             (peg-ir-assert-node-id (:target node) node-count "ref target"))
      :capture (do
                 (peg-ir-assert-node-field node :child)
                 (peg-ir-assert-node-id (:child node) node-count "child"))
      :group (do
               (peg-ir-assert-node-field node :child)
               (peg-ir-assert-node-id (:child node) node-count "child"))
      :replace-cap (do
                     (peg-ir-assert-node-field node :child)
                     (peg-ir-assert-node-field node :subst)
                     (peg-ir-assert-node-id (:child node) node-count "child"))
      :accumulate (do
                    (peg-ir-assert-node-field node :child)
                    (peg-ir-assert-node-id (:child node) node-count "child"))
      :cmt (do
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-field node :f)
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :drop (do
              (peg-ir-assert-node-field node :child)
              (peg-ir-assert-node-id (:child node) node-count "child"))
      :unref (do
               (peg-ir-assert-node-field node :child)
               (peg-ir-assert-node-id (:child node) node-count "child"))
      :only-tags (do
                   (peg-ir-assert-node-field node :child)
                   (peg-ir-assert-node-id (:child node) node-count "child"))
      :nth (do
             (peg-ir-assert-node-field node :child)
             (peg-ir-assert-node-field node :index)
             (peg-ir-assert-node-id (:child node) node-count "child"))
      :number (do
                (peg-ir-assert-node-field node :child)
                (peg-ir-assert-node-id (:child node) node-count "child"))
      :lenprefix (do
                   (peg-ir-assert-node-field node :width)
                   (peg-ir-assert-node-field node :child)
                   (peg-ir-assert-node-id (:child node) node-count "child"))
      :error (if (some? (:child node))
               (peg-ir-assert-node-id (:child node) node-count "child"))
      :constant (peg-ir-assert-node-field node :value)
      :argument (peg-ir-assert-node-field node :index)
      :position nil
      :line nil
      :column nil
      :backref (peg-ir-assert-node-field node :src-tag)
      :backmatch nil
      :uint (do
              (peg-ir-assert-node-field node :width)
              (peg-ir-assert-node-field node :be)
              (peg-ir-assert-node-field node :signed))
      (raise 'peg "unknown IR op: %s" op))))

(defn peg-ir-nodes-map->vector
  [nodes-map (Integer node-count)]
  (let [out nil
        i node-count]
    (while (> i 0)
      (decq i)
      (setq out (cons (get nodes-map i) out)))
    (Vector out)))

(defn peg-ir-build
  "Build normalized IR fields for an already-validated compiled PEG."
  [compiled]
  (let [ctx {:nodes-map {}
             :next-id 0
             :next-slot 0
             :slots {}
             :unresolved nil}
        root-id (peg-ir-compile-pattern! ctx (:root compiled))
        ref-ids {}
        rule-ids {}
        defaults (:defaults compiled)
        rules (:rules compiled)]
    (if (map? rules)
      (for [[name pattern] rules]
        (let [id (peg-ir-compile-pattern! ctx pattern)]
          (put ref-ids name id)
          (put rule-ids name id))))
    (while (:unresolved ctx)
      (let [entry (car (:unresolved ctx))]
        (put ctx :unresolved (cdr (:unresolved ctx)))
        (let [id (:id entry)
              name (:name entry)
              target (if (contains? ref-ids name)
                       (get ref-ids name)
                       nil)]
          (if (and (nil? target) (map? defaults) (contains? defaults name))
            (do
              (setq target (peg-ir-compile-pattern! ctx (get defaults name)))
              (put ref-ids name target)))
          (if (nil? target)
            (raise 'peg "unknown PEG reference: %s" name))
          (let [node (get (:nodes-map ctx) id)]
            (put node :target target)))))
    (let [node-count (:next-id ctx)
          nodes (peg-ir-nodes-map->vector (:nodes-map ctx) node-count)]
      (peg-ir-assert-node-id root-id node-count "root")
      (for [[name id] rule-ids]
        (peg-ir-assert-node-id id node-count (str "rule " name)))
      (let [i 0]
        (while (< i node-count)
          (peg-ir-validate-node (get nodes i) node-count)
          (incq i)))
      {:root-id root-id
       :rule-ids rule-ids
       :nodes nodes})))

(defn peg-attach-ir!
  "Ensure compiled object has validated normalized IR fields."
  [compiled]
  (if (and (vector? (:nodes compiled))
           (integer? (:root-id compiled))
           (map? (:rule-ids compiled)))
    compiled
    (do
      (peg-validate-compiled compiled)
      (let [ir (peg-ir-build compiled)]
        (put compiled :root-id (:root-id ir))
        (put compiled :rule-ids (:rule-ids ir))
        (put compiled :nodes (:nodes ir)))
      compiled)))

(defn compile
  "Compile PEG source into a validated normalized IR artifact."
  [peg-src]
  (if (compiled-peg? peg-src)
    (peg-attach-ir! peg-src)
    (let [grammar (if (map? peg-src) peg-src nil)
          rules (if grammar (clone grammar) {})
          compiled {:kind :compiled-peg
                    :source peg-src
                    :grammar grammar
                    :root (peg-root peg-src)
                    :nodes {}
                    :rules rules
                    :defaults (clone peg-default-aliases)}]
      (peg-attach-ir! compiled))))

(defn ensure-compiled
  "Return compiled PEG artifact for peg source or compiled object."
  [peg]
  (if (compiled-peg? peg)
    (peg-attach-ir! peg)
    (compile peg)))

(defn peg-equal-at?
  "Return true when needle matches source bytes at start."
  [source (Integer start) needle]
  (let [n (len needle)
        i 0
        ok true]
    (while (and ok (< i n))
      (if (= (get source (+ start i)) (get needle i))
        (incq i)
        (setq ok false)))
    ok))

(defn peg-byte-in-string?
  "Return true when byte occurs in string s."
  [(Integer byte) s]
  (let [i 0
        n (len s)
        found false]
    (while (and (not found) (< i n))
      (if (= (get s i) byte)
        (setq found true)
        (incq i)))
    found))

(defn peg-match-string
  [source (Integer pos) (Integer limit) pattern]
  (let [n (len pattern)
        end (+ pos n)]
    (if (> end limit)
      nil
      (if (peg-equal-at? source pos pattern)
        end
        nil))))

(defn peg-match-int
  [(Integer pos) (Integer limit) (Integer n)]
  (if (> n 0)
    (let [end (+ pos n)]
      (if (<= end limit)
        end
        nil))
    (if (= n 0)
      pos
      (let [want (- n)]
        (if (> (+ pos want) limit)
          pos
          nil)))))

(defn peg-match-set
  [source (Integer pos) (Integer limit) set-text]
  (if (>= pos limit)
    nil
    (if (peg-byte-in-string? (get source pos) set-text)
      (+ pos 1)
      nil)))

(defn peg-match-range
  [source (Integer pos) (Integer limit) ranges]
  (if (>= pos limit)
    nil
    (let [byte (get source pos)
          matched false]
      (for [entry ranges]
        (if (not (string? entry))
          (raise 'peg "range expects String entries, got %s" (type entry)))
        (if (not (= (len entry) 2))
          (raise 'peg "range entries must have length 2, got %s" (len entry)))
        (let [lo (get entry 0)
              hi (get entry 1)]
          (if (and (<= lo byte) (<= byte hi))
            (setq matched true))))
      (if matched
        (+ pos 1)
        nil))))

(defn peg-resolve-ref
  "Resolve keyword/symbol reference in grammar/default tables."
  [compiled pattern]
  (let [rules (:rules compiled)
        defaults (:defaults compiled)]
    (if (and (map? rules) (contains? rules pattern))
      (get rules pattern)
      (if (contains? defaults pattern)
        (get defaults pattern)
        nil))))

(defn peg-memo-key
  "Return stable memo key for pattern+position."
  [(Integer pos) pattern]
  (str pos "|" (repr pattern)))

(defn peg-capture-entry
  "Build one capture entry."
  [value &opt (tag nil)]
  {:value value
   :tag tag})

(defn peg-capture-push
  "Push capture entry and update tag stacks."
  [captures (Integer cap-count) tags value tag]
  (if (some? tag)
    (put tags tag (cons value (or (get tags tag) nil))))
  [(cons (peg-capture-entry value tag) captures)
   (+ cap-count 1)
   tags])

(defn peg-capture-pop-tag!
  "Pop one value from a tag stack, removing empty stacks."
  [tags tag]
  (let [vals (get tags tag)]
    (if vals
      (let [rest (cdr vals)]
        (if rest
          (put tags tag rest)
          (del tags tag))))))

(defn peg-captures-pop-to
  "Drop capture entries until target-count is reached."
  [captures (Integer count) target-count tags]
  (let [target (if (nil? target-count) count target-count)]
    (if (not (integer? target))
      (raise 'peg "invalid capture target count type: %s" (type target)))
    (if (< target 0)
      (raise 'peg "invalid capture target count: %s" target-count))
    (if (> target count)
      (raise 'peg "invalid capture target count: %s > %s" target count))
    (let [caps captures
          n count]
      (while (> n target)
        (let [entry (car caps)
              tag (:tag entry)]
          (if (some? tag)
            (peg-capture-pop-tag! tags tag))
          (setq caps (cdr caps))
          (decq n)))
      [caps n tags])))

(defn peg-captures-output
  "Convert internal capture stack to outward-facing Vector."
  [captures]
  (Vector (map (fn [entry] (:value entry)) (nreverse captures))))

(defn peg-produced-count
  "Return number of captures produced since start-cap."
  [(Integer cap-count) (Integer start-cap) op]
  (let [produced (- cap-count start-cap)]
    (if (< produced 0)
      (raise 'peg "invalid %s capture state: %s" op produced))
    produced))

(defn peg-capture-values-since
  "Return produced capture values in chronological order."
  [captures (Integer produced)]
  (let [vals nil
        it captures
        i 0]
    (while (< i produced)
      (setq vals (cons (:value (car it)) vals))
      (setq it (cdr it))
      (incq i))
    vals))

(defn peg-capture-entries-since
  "Return produced capture entries in chronological order."
  [captures (Integer produced)]
  (let [entries nil
        it captures
        i 0]
    (while (< i produced)
      (setq entries (cons (car it) entries))
      (setq it (cdr it))
      (incq i))
    entries))

(defn peg-latest-untagged-capture
  "Return latest untagged capture value, or nil."
  [captures (Integer produced)]
  (let [it captures
        i 0
        found nil]
    (while (and (< i produced) (nil? found))
      (let [entry (car it)]
        (if (nil? (:tag entry))
          (setq found (:value entry))
          (do
            (setq it (cdr it))
            (incq i)))))
    found))

(defn peg-read-uint-le-at
  "Decode little-endian unsigned integer from source at pos."
  [source (Integer pos) (Integer limit) (Integer width)]
  (if (<= width 0)
    (raise 'peg "integer width should be positive, got %s" width))
  (let [end (+ pos width)]
    (if (> end limit)
      nil
      (let [result 0
            i 0
            shift 0]
        (while (< i width)
          (let [byte (get source (+ pos i))]
            (setq result (bit-or result (bit-shift-left byte shift))))
          (setq shift (+ shift 8))
          (incq i))
        result))))

(defn peg-read-uint-be-at
  "Decode big-endian unsigned integer from source at pos."
  [source (Integer pos) (Integer limit) (Integer width)]
  (if (<= width 0)
    (raise 'peg "integer width should be positive, got %s" width))
  (let [end (+ pos width)]
    (if (> end limit)
      nil
      (let [result 0
            i 0]
        (while (< i width)
          (let [byte (get source (+ pos i))]
            (setq result (bit-or (bit-shift-left result 8) byte)))
          (incq i))
        result))))

(defn peg-read-int-be-at
  "Decode big-endian signed integer from source at pos."
  [source (Integer pos) (Integer limit) (Integer width)]
  (if (<= width 0)
    (raise 'peg "integer width should be positive, got %s" width))
  (let [end (+ pos width)]
    (if (> end limit)
      nil
      (let [sign-byte (get source pos)
            signed? (not= (bit-and sign-byte 0x80) 0)
            result (if signed? -1 0)
            i 0]
        (while (< i width)
          (setq result (bit-or (bit-shift-left result 8)
                               (get source (+ pos i))))
          (incq i))
        result))))

(defn peg-read-int-le-at
  "Decode little-endian signed integer from source at pos."
  [source (Integer pos) (Integer limit) (Integer width)]
  (if (<= width 0)
    (raise 'peg "integer width should be positive, got %s" width))
  (let [end (+ pos width)]
    (if (> end limit)
      nil
      (let [sign-byte (get source (- end 1))
            signed? (not= (bit-and sign-byte 0x80) 0)
            result (if signed? -1 0)
            i (- width 1)]
        (while (>= i 0)
          (setq result (bit-or (bit-shift-left result 8)
                               (get source (+ pos i))))
          (decq i))
        result))))

(defn peg-digit-value
  "Return numeric value for ASCII digit byte, else nil."
  [(Integer byte)]
  (if (and (<= 48 byte) (<= byte 57))
    (- byte 48)
    (if (and (<= 65 byte) (<= byte 90))
      (+ 10 (- byte 65))
      (if (and (<= 97 byte) (<= byte 122))
        (+ 10 (- byte 97))
        nil))))

(defn peg-string-has-float-marker?
  [text]
  (let [n (len text)
        i 0
        found false]
    (while (and (not found) (< i n))
      (let [byte (get text i)]
        (if (or (= byte 46) (= byte 69) (= byte 101))
          (setq found true)
          (incq i))))
    found))

(defn peg-parse-int-base
  "Parse signed integer text in base 2..36."
  [text (Integer base)]
  (if (or (< base 2) (> base 36))
    (raise 'peg "number base should be in 2..36, got %s" base))
  (let [n (len text)
        i 0
        sign 1
        value 0
        digits 0]
    (if (= n 0)
      (raise 'peg "cannot parse empty number"))
    (if (= (get text 0) 45)
      (do
        (setq sign -1)
        (setq i 1))
      (if (= (get text 0) 43)
        (setq i 1)))
    (while (< i n)
      (let [digit (peg-digit-value (get text i))]
        (if (or (nil? digit) (>= digit base))
          (raise 'peg "invalid digit in base %s number: %s" base text))
        (setq value (+ (* value base) digit))
        (incq digits)
        (incq i)))
    (if (= digits 0)
      (raise 'peg "invalid number: %s" text))
    (* sign value)))

(defn peg-parse-number
  "Parse text into Integer/Float, optionally with base."
  [text base]
  (if (some? base)
    (if (not (integer? base))
      (raise 'peg "number base should be Integer, got %s" (type base))
      (peg-parse-int-base text base))
    (if (peg-string-has-float-marker? text)
      (Float text)
      (Integer text))))

(defn peg-line-column-at
  "Return [line column] at byte position pos (1-based line/column)."
  [source (Integer pos)]
  (let [line 1
        col 1
        i 0]
    (while (< i pos)
      (if (= (get source i) 10)
        (do
          (incq line)
          (setq col 1))
        (incq col))
      (incq i))
    [line col]))

(defn peg-eval-iter-return-state
  "Return evaluator state bundle in :return mode."
  [captures (Integer cap-count) tags ret]
  {:captures captures
   :cap-count cap-count
   :tags tags
   :ret ret
   :mode :return})

(defn peg-eval-iter-return-pushed
  "Push capture value and return updated evaluator state bundle."
  [captures (Integer cap-count) tags value tag ret]
  (let [pushed (peg-capture-push captures cap-count tags value tag)]
    (peg-eval-iter-return-state (get pushed 0)
                                (get pushed 1)
                                (get pushed 2)
                                ret)))

(defn peg-ir-node-at
  "Return normalized IR node at node-id, raising on invalid shape/index."
  [compiled node-id]
  (let [nodes (:nodes compiled)]
    (if (not (vector? nodes))
      (raise 'peg "compiled PEG :nodes should be Vector, got %s" (type nodes)))
    (if (not (integer? node-id))
      (raise 'peg "IR node id should be Integer, got %s" (type node-id)))
    (if (or (< node-id 0) (>= node-id (len nodes)))
      (raise 'peg "IR node id %s out of range (0..%s)" node-id (- (len nodes) 1)))
    (get nodes node-id)))

(defn peg-eval-ir-step
  "Evaluate one IR node in :eval mode and return updated evaluator state."
  [compiled source (Integer cur-pos) (Integer limit) (Integer cur-node)
   stack captures (Integer cap-count) tags run-args]
  (let [node (peg-ir-node-at compiled cur-node)
        op (:op node)
        next-stack stack
        next-pattern cur-node
        next-pos cur-pos
        next-mode :return
        next-ret nil
        next-captures captures
        next-cap-count cap-count
        next-tags tags]
    (case op
      :lit
      (setq next-ret (peg-match-string source cur-pos limit (:text node)))
      :bytes
      (setq next-ret (peg-match-int cur-pos limit (:count node)))
      :bool
      (setq next-ret (if (:value node)
                       (if (< cur-pos limit)
                         (+ cur-pos 1)
                         nil)
                       nil))
      :ref
      (let [target (:target node)]
        (if (some? target)
          (do
            (setq next-pattern target)
            (setq next-mode :eval))
          (raise 'peg "unknown PEG reference: %s" (:name node))))
      :or
      (let [args (:children node)
            argc (len args)]
        (if (= argc 0)
          (setq next-ret nil)
          (do
            (setq next-stack (cons {:kind :or
                                    :args args
                                    :index 1
                                    :start cur-pos
                                    :cap cap-count}
                                   next-stack))
            (setq next-pattern (get args 0))
            (setq next-mode :eval))))
      :and
      (let [args (:children node)
            argc (len args)]
        (if (= argc 0)
          (setq next-ret cur-pos)
          (do
            (setq next-stack (cons {:kind :and
                                    :args args
                                    :index 1
                                    :cap cap-count}
                                   next-stack))
            (setq next-pattern (get args 0))
            (setq next-mode :eval))))
      :any
      (do
        (setq next-stack (cons {:kind :any
                                :child (:child node)
                                :cur cur-pos
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :some
      (do
        (setq next-stack (cons {:kind :some
                                :child (:child node)
                                :start cur-pos
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :repeat
      (let [n (:count node)
            child (:child node)]
        (if (< n 0)
          (raise 'peg "repeat count must be non-negative, got %s" n))
        (if (= n 0)
          (setq next-ret cur-pos)
          (do
            (setq next-stack (cons {:kind :repeat
                                    :child child
                                    :remaining n
                                    :start-cap cap-count}
                                   next-stack))
            (setq next-pattern child)
            (setq next-mode :eval))))
      :at-most
      (let [n (:count node)
            child (:child node)]
        (if (< n 0)
          (raise 'peg "at-most count must be non-negative, got %s" n))
        (if (= n 0)
          (setq next-ret cur-pos)
          (do
            (setq next-stack (cons {:kind :at-most
                                    :child child
                                    :remaining n
                                    :cur cur-pos
                                    :cap cap-count}
                                   next-stack))
            (setq next-pattern child)
            (setq next-mode :eval))))
      :at-least
      (let [n (:count node)
            child (:child node)]
        (if (< n 0)
          (raise 'peg "at-least count must be non-negative, got %s" n))
        (if (= n 0)
          (do
            (setq next-stack (cons {:kind :any
                                    :child child
                                    :cur cur-pos
                                    :cap cap-count}
                                   next-stack))
            (setq next-pattern child)
            (setq next-mode :eval))
          (do
            (setq next-stack (cons {:kind :at-least
                                    :child child
                                    :remaining n
                                    :start-cap cap-count}
                                   next-stack))
            (setq next-pattern child)
            (setq next-mode :eval))))
      :between
      (let [nmin (:min node)
            nmax (:max node)
            child (:child node)]
        (if (< nmin 0)
          (raise 'peg "between min must be non-negative, got %s" nmin))
        (if (< nmax 0)
          (raise 'peg "between max must be non-negative, got %s" nmax))
        (if (> nmin nmax)
          (raise 'peg "between expects min <= max, got %s > %s" nmin nmax))
        (if (= nmax 0)
          (setq next-ret cur-pos)
          (if (= nmin 0)
            (do
              (setq next-stack (cons {:kind :at-most
                                      :child child
                                      :remaining nmax
                                      :cur cur-pos
                                      :cap cap-count}
                                     next-stack))
              (setq next-pattern child)
              (setq next-mode :eval))
            (do
              (setq next-stack (cons {:kind :between
                                      :child child
                                      :min-remaining nmin
                                      :max-remaining nmax
                                      :start-cap cap-count}
                                     next-stack))
              (setq next-pattern child)
              (setq next-mode :eval)))))
      :if
      (do
        (setq next-stack (cons {:kind :if
                                :phase :cond
                                :start cur-pos
                                :cond-neg (:neg node)
                                :child (:child node)
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:cond node))
        (setq next-mode :eval))
      :not
      (do
        (setq next-stack (cons {:kind :not
                                :start cur-pos
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :look
      (let [offset (:offset node)
            child (:child node)
            target (+ cur-pos offset)]
        (if (< target 0)
          (setq next-ret nil)
          (do
            (setq next-stack (cons {:kind :look
                                    :start cur-pos
                                    :cap cap-count}
                                   next-stack))
            (setq next-pattern child)
            (setq next-pos target)
            (setq next-mode :eval))))
      :sub
      (do
        (setq next-stack (cons {:kind :sub
                                :phase :window
                                :start cur-pos
                                :cap cap-count
                                :limit limit
                                :child (:child node)}
                               next-stack))
        (setq next-pattern (:window node))
        (setq next-mode :eval))
      :opt
      (do
        (setq next-stack (cons {:kind :opt
                                :start cur-pos
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :capture
      (do
        (setq next-stack (cons {:kind :capture
                                :start cur-pos
                                :cap cap-count
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :group
      (do
        (setq next-stack (cons {:kind :group
                                :cap cap-count
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :replace-cap
      (do
        (setq next-stack (cons {:kind :replace-cap
                                :cap cap-count
                                :subst (:subst node)
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :accumulate
      (do
        (setq next-stack (cons {:kind :accumulate
                                :cap cap-count
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :cmt
      (do
        (setq next-stack (cons {:kind :cmt
                                :cap cap-count
                                :f (:f node)
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :drop
      (do
        (setq next-stack (cons {:kind :drop
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :unref
      (do
        (setq next-stack (cons {:kind :unref
                                :cap cap-count
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :only-tags
      (do
        (setq next-stack (cons {:kind :only-tags
                                :cap cap-count}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :nth
      (do
        (setq next-stack (cons {:kind :nth
                                :cap cap-count
                                :index (:index node)
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :number
      (do
        (setq next-stack (cons {:kind :number
                                :cap cap-count
                                :start cur-pos
                                :base (:base node)
                                :tag (:tag node)}
                               next-stack))
        (setq next-pattern (:child node))
        (setq next-mode :eval))
      :lenprefix
      (let [width (:width node)
            child (:child node)
            window-size (peg-read-uint-le-at source cur-pos limit width)]
        (if (nil? window-size)
          (setq next-ret nil)
          (let [start (+ cur-pos width)
                end (+ (+ cur-pos width) window-size)]
            (if (> end limit)
              (setq next-ret nil)
              (do
                (setq next-stack (cons {:kind :window
                                        :cap cap-count
                                        :limit limit}
                                       next-stack))
                (setq next-pattern child)
                (setq next-pos start)
                (setq limit end)
                (setq next-mode :eval))))))
      :error
      (if (some? (:child node))
        (do
          (setq next-stack (cons {:kind :error
                                  :cap cap-count
                                  :start cur-pos}
                                 next-stack))
          (setq next-pattern (:child node))
          (setq next-mode :eval))
        (let [[line col] (peg-line-column-at source cur-pos)]
          (raise 'peg "parse error at line %s column %s" line col)))
      :constant
      (let [state (peg-eval-iter-return-pushed next-captures
                                               next-cap-count
                                               next-tags
                                               (:value node)
                                               (:tag node)
                                               cur-pos)]
        (setq next-captures (:captures state))
        (setq next-cap-count (:cap-count state))
        (setq next-tags (:tags state))
        (setq next-ret (:ret state))
        (setq next-mode (:mode state)))
      :argument
      (let [index (:index node)
            out-tag (:tag node)]
        (if (not (integer? index))
          (raise 'peg "argument index should be Integer, got %s" (type index)))
        (if (< index 0)
          (raise 'peg "argument index should be non-negative, got %s" index))
        (let [argcnt (len run-args)
              value (if (< index argcnt)
                      (get run-args index)
                      nil)
              state (peg-eval-iter-return-pushed next-captures
                                                 next-cap-count
                                                 next-tags
                                                 value
                                                 out-tag
                                                 cur-pos)]
          (setq next-captures (:captures state))
          (setq next-cap-count (:cap-count state))
          (setq next-tags (:tags state))
          (setq next-ret (:ret state))
          (setq next-mode (:mode state))))
      :uint
      (let [width (:width node)
            value (if (:signed node)
                    (if (:be node)
                      (peg-read-int-be-at source cur-pos limit width)
                      (peg-read-int-le-at source cur-pos limit width))
                    (if (:be node)
                      (peg-read-uint-be-at source cur-pos limit width)
                      (peg-read-uint-le-at source cur-pos limit width)))]
        (if (nil? value)
          (setq next-ret nil)
          (let [state (peg-eval-iter-return-pushed next-captures
                                                   next-cap-count
                                                   next-tags
                                                   value
                                                   (:tag node)
                                                   (+ cur-pos width))]
            (setq next-captures (:captures state))
            (setq next-cap-count (:cap-count state))
            (setq next-tags (:tags state))
            (setq next-ret (:ret state))
            (setq next-mode (:mode state)))))
      :position
      (let [state (peg-eval-iter-return-pushed next-captures
                                               next-cap-count
                                               next-tags
                                               cur-pos
                                               (:tag node)
                                               cur-pos)]
        (setq next-captures (:captures state))
        (setq next-cap-count (:cap-count state))
        (setq next-tags (:tags state))
        (setq next-ret (:ret state))
        (setq next-mode (:mode state)))
      :line
      (let [[line _col] (peg-line-column-at source cur-pos)
            state (peg-eval-iter-return-pushed next-captures
                                               next-cap-count
                                               next-tags
                                               line
                                               (:tag node)
                                               cur-pos)]
        (setq next-captures (:captures state))
        (setq next-cap-count (:cap-count state))
        (setq next-tags (:tags state))
        (setq next-ret (:ret state))
        (setq next-mode (:mode state)))
      :column
      (let [[_line col] (peg-line-column-at source cur-pos)
            state (peg-eval-iter-return-pushed next-captures
                                               next-cap-count
                                               next-tags
                                               col
                                               (:tag node)
                                               cur-pos)]
        (setq next-captures (:captures state))
        (setq next-cap-count (:cap-count state))
        (setq next-tags (:tags state))
        (setq next-ret (:ret state))
        (setq next-mode (:mode state)))
      :backref
      (let [src-tag (:src-tag node)
            out-tag (:out-tag node)
            vals (get next-tags src-tag)]
        (if vals
          (let [state (peg-eval-iter-return-pushed next-captures
                                                   next-cap-count
                                                   next-tags
                                                   (car vals)
                                                   out-tag
                                                   cur-pos)]
            (setq next-captures (:captures state))
            (setq next-cap-count (:cap-count state))
            (setq next-tags (:tags state))
            (setq next-ret (:ret state))
            (setq next-mode (:mode state)))
          (setq next-ret nil)))
      :backmatch
      (let [probe (if (some? (:src-tag node))
                    (let [vals (get next-tags (:src-tag node))]
                      (if vals
                        (car vals)
                        nil))
                    (if (> next-cap-count 0)
                      (peg-latest-untagged-capture next-captures next-cap-count)
                      nil))]
        (if (nil? probe)
          (setq next-ret nil)
          (setq next-ret (peg-match-string source cur-pos limit (str probe)))))
      :range
      (setq next-ret (peg-match-range source cur-pos limit (:ranges node)))
      :set
      (setq next-ret (peg-match-set source cur-pos limit (:charset node)))
      (raise 'peg "unsupported IR op: %s" op))
    {:stack next-stack
     :cur-pattern next-pattern
     :cur-pos next-pos
     :mode next-mode
     :ret next-ret
     :captures next-captures
     :cap-count next-cap-count
     :tags next-tags}))

(defn peg-eval-iter
  "Evaluate pattern at pos with an explicit interpreter stack."
  [compiled source (Integer pos) (Integer limit) memo active pattern captures (Integer cap-count) tags run-args use-memo]
  (let [stack nil
        cur-pattern pattern
        cur-pos pos
        mode :eval
        ret nil
        done false
        result nil]
    (while (not done)
      (if (= mode :eval)
        (let [key (peg-memo-key cur-pos cur-pattern)]
          (if (and use-memo (contains? memo key))
            (do
              (setq ret (get memo key))
              (setq mode :return))
            (if (contains? active key)
              (do
                (setq ret nil)
                (setq mode :return))
              (do
                (put active key true)
                (setq stack (cons {:kind :finalize :key key :memo use-memo} stack))
                (let [step (peg-eval-ir-step compiled
                                                  source
                                                  cur-pos
                                                  limit
                                                  cur-pattern
                                                  stack
                                                  captures
                                                  cap-count
                                                  tags
                                                  run-args)]
                  (setq stack (:stack step))
                  (setq cur-pattern (:cur-pattern step))
                  (setq cur-pos (:cur-pos step))
                  (setq mode (:mode step))
                  (setq ret (:ret step))
                  (setq captures (:captures step))
                  (setq cap-count (:cap-count step))
                  (setq tags (:tags step)))))))
        (if (nil? stack)
          (do
            (setq result ret)
            (setq done true))
          (let [frame (car stack)]
            (setq stack (cdr stack))
            (case (:kind frame)
              :finalize
              (do
                (del active (:key frame))
                (if (:memo frame)
                  (put memo (:key frame) ret))
                (setq mode :return))
              :or
              (let [args (:args frame)
                    idx (:index frame)
                    start (:start frame)
                    cap (:cap frame)]
                (if (some? ret)
                  (setq mode :return)
                  (if (< idx (len args))
                    (do
                      (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                        (setq captures caps)
                        (setq cap-count n)
                        (setq tags t))
                      (setq stack (cons {:kind :or
                                         :args args
                                         :index (+ idx 1)
                                         :start start
                                         :cap cap}
                                        stack))
                      (setq cur-pattern (get args idx))
                      (setq cur-pos start)
                      (setq mode :eval))
                    (do
                      (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                        (setq captures caps)
                        (setq cap-count n)
                        (setq tags t))
                      (setq ret nil)
                      (setq mode :return)))))
              :and
              (let [args (:args frame)
                    idx (:index frame)
                    cap (:cap frame)]
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq mode :return))
                  (if (< idx (len args))
                    (do
                      (setq stack (cons {:kind :and
                                         :args args
                                         :index (+ idx 1)
                                         :cap cap}
                                        stack))
                      (setq cur-pattern (get args idx))
                      (setq cur-pos ret)
                      (setq mode :eval))
                    (setq mode :return))))
              :any
              (let [child (:child frame)
                    cur (:cur frame)
                    cap (:cap frame)]
                (if (or (nil? ret) (= ret cur))
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq ret cur)
                    (setq mode :return))
                  (do
                    (setq stack (cons {:kind :any
                                       :child child
                                       :cur ret
                                       :cap cap-count}
                                      stack))
                    (setq cur-pattern child)
                    (setq cur-pos ret)
                    (setq mode :eval))))
              :some
              (let [child (:child frame)
                    cap (:cap frame)]
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq mode :return))
                  (do
                    (setq stack (cons {:kind :any
                                       :child child
                                       :cur ret
                                       :cap cap-count}
                                      stack))
                    (setq cur-pattern child)
                    (setq cur-pos ret)
                    (setq mode :eval))))
              :repeat
              (let [child (:child frame)
                    remaining (:remaining frame)
                    start-cap (:start-cap frame)]
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count start-cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq mode :return))
                  (if (> remaining 1)
                    (do
                      (setq stack (cons {:kind :repeat
                                         :child child
                                         :remaining (- remaining 1)
                                         :start-cap start-cap}
                                        stack))
                      (setq cur-pattern child)
                      (setq cur-pos ret)
                      (setq mode :eval))
                    (setq mode :return))))
              :at-most
              (let [child (:child frame)
                    remaining (:remaining frame)
                    cur (:cur frame)
                    cap (:cap frame)]
                (if (or (nil? ret) (= ret cur))
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq ret cur)
                    (setq mode :return))
                  (if (> remaining 1)
                    (do
                      (setq stack (cons {:kind :at-most
                                         :child child
                                         :remaining (- remaining 1)
                                         :cur ret
                                         :cap cap-count}
                                        stack))
                      (setq cur-pattern child)
                      (setq cur-pos ret)
                      (setq mode :eval))
                    (setq mode :return))))
              :at-least
              (let [child (:child frame)
                    remaining (:remaining frame)
                    start-cap (:start-cap frame)]
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count start-cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq mode :return))
                  (if (> remaining 1)
                    (do
                      (setq stack (cons {:kind :at-least
                                         :child child
                                         :remaining (- remaining 1)
                                         :start-cap start-cap}
                                        stack))
                      (setq cur-pattern child)
                      (setq cur-pos ret)
                      (setq mode :eval))
                    (do
                      (setq stack (cons {:kind :any
                                         :child child
                                         :cur ret
                                         :cap cap-count}
                                        stack))
                      (setq cur-pattern child)
                      (setq cur-pos ret)
                      (setq mode :eval)))))
              :between
              (let [child (:child frame)
                    min-remaining (:min-remaining frame)
                    max-remaining (:max-remaining frame)
                    start-cap (:start-cap frame)]
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count start-cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq mode :return))
                  (let [min-next (- min-remaining 1)
                        max-next (- max-remaining 1)]
                    (if (> min-next 0)
                      (do
                        (setq stack (cons {:kind :between
                                           :child child
                                           :min-remaining min-next
                                           :max-remaining max-next
                                           :start-cap start-cap}
                                          stack))
                        (setq cur-pattern child)
                        (setq cur-pos ret)
                        (setq mode :eval))
                      (if (= max-next 0)
                        (setq mode :return)
                        (do
                          (setq stack (cons {:kind :at-most
                                             :child child
                                             :remaining max-next
                                             :cur ret
                                             :cap cap-count}
                                            stack))
                          (setq cur-pattern child)
                          (setq cur-pos ret)
                          (setq mode :eval)))))))
              :if
              (let [start (:start frame)
                    child (:child frame)
                    neg (:cond-neg frame)
                    phase (:phase frame)
                    cap (:cap frame)]
                (if (or (nil? phase) (= phase :cond))
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count cap tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (if neg
                      (if (some? ret)
                        (do
                          (setq ret nil)
                          (setq mode :return))
                        (do
                          (setq stack (cons {:kind :if
                                             :phase :child
                                             :start start
                                             :cond-neg neg
                                             :child child
                                             :cap cap-count}
                                            stack))
                          (setq cur-pattern child)
                          (setq cur-pos start)
                          (setq mode :eval)))
                      (if (some? ret)
                        (do
                          (setq stack (cons {:kind :if
                                             :phase :child
                                             :start start
                                             :cond-neg neg
                                             :child child
                                             :cap cap-count}
                                            stack))
                          (setq cur-pattern child)
                          (setq cur-pos start)
                          (setq mode :eval))
                        (do
                          (setq ret nil)
                          (setq mode :return)))))
                  (if (= phase :child)
                    (setq mode :return)
                    (raise 'peg "unknown peg if frame phase: %s" phase))))
              :not
              (do
                (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                  (setq captures caps)
                  (setq cap-count n)
                  (setq tags t))
                (setq ret (if (some? ret) nil (:start frame)))
                (setq mode :return))
              :look
              (do
                (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                  (setq captures caps)
                  (setq cap-count n)
                  (setq tags t))
                (setq ret (if (some? ret) (:start frame) nil))
                (setq mode :return))
              :opt
              (do
                (if (nil? ret)
                  (do
                    (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                      (setq captures caps)
                      (setq cap-count n)
                      (setq tags t))
                    (setq ret (:start frame))))
                (setq mode :return))
              :sub
              (let [phase (:phase frame)]
                (if (= phase :window)
                  (if (nil? ret)
                    (do
                      (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                        (setq captures caps)
                        (setq cap-count n)
                        (setq tags t))
                      (setq mode :return))
                    (do
                      (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                        (setq captures caps)
                        (setq cap-count n)
                        (setq tags t))
                      (setq stack (cons {:kind :sub
                                         :phase :child
                                         :cap cap-count
                                         :limit (:limit frame)}
                                        stack))
                      (setq cur-pattern (:child frame))
                      (setq cur-pos (:start frame))
                      (setq limit ret)
                      (setq mode :eval)))
                  (if (= phase :child)
                    (do
                      (setq limit (:limit frame))
                      (if (nil? ret)
                        (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                          (setq captures caps)
                          (setq cap-count n)
                          (setq tags t)))
                      (setq mode :return))
                    (raise 'peg "unknown peg sub frame phase: %s" phase))))
              :window
              (do
                (setq limit (:limit frame))
                (if (nil? ret)
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t)))
                (setq mode :return))
              :replace-cap
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [produced (peg-produced-count cap-count (:cap frame) 'replace)
                      values (peg-capture-values-since captures produced)
                      latest (if (> produced 0) (:value (car captures)) nil)
                      subst (:subst frame)
                      out (if (map? subst)
                            (if (> produced 0)
                              (get subst latest)
                              nil)
                            (if (function? subst)
                              (apply subst (Vector values))
                              subst))]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (let [pushed (peg-capture-push captures cap-count tags out (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :accumulate
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [produced (peg-produced-count cap-count (:cap frame) 'accumulate)
                      values (peg-capture-values-since captures produced)]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (let [pushed (peg-capture-push captures
                                                 cap-count
                                                 tags
                                                 (Vector values)
                                                 (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :cmt
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [produced (peg-produced-count cap-count (:cap frame) 'cmt)
                      values (peg-capture-values-since captures produced)
                      out (apply (:f frame) (Vector values))]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (let [pushed (peg-capture-push captures cap-count tags out (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :only-tags
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [produced (peg-produced-count cap-count (:cap frame) 'only-tags)
                      entries (peg-capture-entries-since captures produced)]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (for [entry entries]
                    (if (some? (:tag entry))
                      (let [pushed (peg-capture-push captures
                                                     cap-count
                                                     tags
                                                     (:value entry)
                                                     (:tag entry))]
                        (setq captures (get pushed 0))
                        (setq cap-count (get pushed 1))
                        (setq tags (get pushed 2)))))
                  (setq mode :return)))
              :nth
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [produced (peg-produced-count cap-count (:cap frame) 'nth)
                      values (Vector (peg-capture-values-since captures produced))
                      nvals (len values)
                      index (:index frame)
                      idx (if (< index 0)
                            (+ nvals index)
                            index)
                      value (if (or (< idx 0) (>= idx nvals))
                              nil
                              (get values idx))]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (let [pushed (peg-capture-push captures cap-count tags value (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :number
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [text (peg-slice-source source (:start frame) ret)
                      value (peg-parse-number text (:base frame))]
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (let [pushed (peg-capture-push captures cap-count tags value (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :error
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [[line col] (peg-line-column-at source (:start frame))
                      produced (peg-produced-count cap-count (:cap frame) 'error)
                      msg (if (> produced 0)
                            (str (:value (car captures)))
                            nil)]
                  (if (some? msg)
                    (raise 'peg "parse error at line %s column %s: %s" line col msg)
                    (raise 'peg "parse error at line %s column %s" line col))))
              :capture
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (do
                  (let [pushed (peg-capture-push captures
                                                 cap-count
                                                 tags
                                                 (peg-slice-source source (:start frame) ret)
                                                 (:tag frame))]
                    (setq captures (get pushed 0))
                    (setq cap-count (get pushed 1))
                    (setq tags (get pushed 2)))
                  (setq mode :return)))
              :group
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (let [start-cap (:cap frame)
                      produced (- cap-count start-cap)]
                  (if (< produced 0)
                    (raise 'peg "invalid capture group state: %s" produced))
                  (let [vals nil
                        caps captures
                        n cap-count
                        i 0]
                    (while (< i produced)
                      (setq vals (cons (:value (car caps)) vals))
                      (setq caps (cdr caps))
                      (decq n)
                      (incq i))
                    (setq captures caps)
                    (setq cap-count n)
                    (let [pushed (peg-capture-push captures
                                                   cap-count
                                                   tags
                                                   (Vector vals)
                                                   (:tag frame))]
                      (setq captures (get pushed 0))
                      (setq cap-count (get pushed 1))
                      (setq tags (get pushed 2)))
                    (setq mode :return))))
              :drop
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return)))
              :unref
              (if (nil? ret)
                (do
                  (let [[caps n t] (peg-captures-pop-to captures cap-count (:cap frame) tags)]
                    (setq captures caps)
                    (setq cap-count n)
                    (setq tags t))
                  (setq mode :return))
                (do
                  (let [produced (- cap-count (:cap frame))
                        i 0
                        it captures
                        scope-tag (:tag frame)]
                    (while (< i produced)
                      (let [entry (car it)
                            etag (:tag entry)]
                        (if (and (some? etag)
                                 (or (nil? scope-tag) (= etag scope-tag)))
                          (do
                            (peg-capture-pop-tag! tags etag)
                            (put entry :tag nil))))
                      (setq it (cdr it))
                      (incq i)))
                  (setq mode :return)))
            (raise 'peg "unknown peg frame kind: %s (type %s frame %s)"
                   (:kind frame)
                   (type (:kind frame))
                   frame))))))
    {:end result
     :captures captures
     :cap-count cap-count
     :tags tags}))

(defn peg-resolve-ir
  [compiled state (Integer pos) (Integer node-id)]
  (let [res (peg-eval-iter compiled
                           (:source state)
                           pos
                           (:limit state)
                           (:memo state)
                           (:active state)
                           node-id
                           (:captures state)
                           (:cap-count state)
                           (:tags state)
                           (:args state)
                           false)]
    (put state :captures (:captures res))
    (put state :cap-count (:cap-count res))
    (put state :tags (:tags res))
    (:end res)))

(defn peg-root-eval-at-ir
  "Evaluate compiled PEG IR root at source position pos."
  [compiled state (Integer pos)]
  (peg-resolve-ir compiled state pos (:root-id compiled)))

(defn peg-root-eval-at
  "Evaluate compiled PEG root at source position pos."
  [compiled state (Integer pos)]
  (peg-root-eval-at-ir compiled state pos))

(defn peg-safe-root-eval-at
  "Evaluate root, translating evaluator recursion overflow into a peg error."
  [compiled state (Integer pos)]
  (catch [ex
          (case ex
            (cons 'eval msg)
            (if (= msg "infinite recursion")
              (raise 'peg "recursive PEG evaluation exceeded LANGSAM_MAX_EVAL_DEPTH")
              (throw ex))
            (throw ex))]
    (peg-root-eval-at compiled state pos)))

(defn peg-make-runstate
  "Build read-only parse view for a stream-backed run."
  [input args]
  (let [s (stream input)
        source (s.peek)]
    {:stream s
     :source source
     :limit (len source)
     :args (Vector args)
     :memo {}
     :active {}
     :captures nil
     :cap-count 0
     :tags {}}))

(defn peg-reset-captures!
  "Reset runstate capture output."
  [state]
  (put state :captures nil)
  (put state :cap-count 0)
  (put state :tags {}))

(defn peg-current-captures
  "Return current runstate captures as a Vector."
  [state]
  (peg-captures-output (:captures state)))

(defn peg-consume-prefix!
  "Consume n bytes from the underlying stream."
  [state (Integer n)]
  (if (< n 0)
    (raise 'peg "cannot consume negative byte count: %s" n))
  (if (> n 0)
    (let [s (:stream state)]
      (s.read n))))

(defn peg-consume-eof!
  "Consume remaining stream input."
  [state]
  (let [s (:stream state)]
    (s.read 0)))

(defn peg-find-span
  "Return [start end] for first match at/after start, else nil."
  [compiled state (Integer start)]
  (let [limit (:limit state)]
    (if (> start limit)
      nil
      (do
        (let [i start
              found nil]
          (while (and (nil? found) (<= i limit))
            (do
              (peg-reset-captures! state)
              (let [end (peg-safe-root-eval-at compiled state i)]
                (if (some? end)
                  (setq found [i end])
                  (incq i)))))
          found)))))

(defn peg-slice-source
  "Return String slice [begin,end) from source."
  [source (Integer begin) (Integer end)]
  (if (= begin end)
    ""
    (String (slice source begin end))))

(defn peg-next-search-pos
  "Return next scan position, forcing progress on zero-width matches."
  [(Integer begin) (Integer end)]
  (if (= begin end)
    (+ begin 1)
    end))

(defn peg-join-parts
  "Join reversed list of string parts into one String."
  [rev-parts]
  (let [out ""
        it (iter (nreverse rev-parts))]
    (while it
      (setq out (+ out @it))
      (setq it (next it)))
    out))

(defn peg-replace-scan
  "Scan and replace matches from start, consuming input to EOF."
  [compiled state (Integer start) replacement-fn]
  (let [source (:source state)
        limit (:limit state)
        parts nil
        copy-pos 0
        search-pos start
        done false]
    (if (> start 0)
      (do
        (setq parts (cons (peg-slice-source source 0 start) parts))
        (setq copy-pos start)))
    (while (and (not done) (<= search-pos limit))
      (let [span (peg-find-span compiled state search-pos)]
        (if (nil? span)
          (setq done true)
          (let [[begin end] span]
            (if (> begin copy-pos)
              (setq parts (cons (peg-slice-source source copy-pos begin) parts)))
            (setq parts (cons (str (replacement-fn (peg-slice-source source begin end)
                                                   begin
                                                   end))
                              parts))
            (setq copy-pos end)
            (setq search-pos (peg-next-search-pos begin end))))))
    (if (< copy-pos limit)
      (setq parts (cons (peg-slice-source source copy-pos limit) parts)))
    (peg-consume-eof! state)
    (peg-join-parts parts)))

(defn match
  "Run a compiled/source PEG against input."
  [peg input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)
        source-len (:limit state)]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (if (> start source-len)
      nil
      (do
        (peg-reset-captures! state)
        (let [matched-end (peg-safe-root-eval-at compiled state start)]
        (if (nil? matched-end)
          nil
          (do
            (peg-consume-prefix! state matched-end)
            (peg-current-captures state))))))))

(defn find
  "Find first PEG match in input; consumes scanned input."
  [peg input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (let [span (peg-find-span compiled state start)]
      (if span
        (let [[_begin end] span]
          (peg-consume-prefix! state end)
          (peg-current-captures state))
        (do
          (peg-consume-eof! state)
          nil)))))

(defn find-all
  "Find all PEG matches in input; consumes to EOF."
  [peg input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)
        limit (:limit state)]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (if (> start limit)
      (do
        (peg-consume-eof! state)
        [])
      (let [matches nil
            search-pos start]
        (while (<= search-pos limit)
          (let [span (peg-find-span compiled state search-pos)]
            (if (nil? span)
              (setq search-pos (+ limit 1))
              (let [[begin end] span]
                (setq matches (cons (peg-current-captures state) matches))
                (setq search-pos (peg-next-search-pos begin end))))))
        (peg-consume-eof! state)
        (Vector (nreverse matches))))))

(defn replace
  "Replace PEG matches with subst value/function; consumes to EOF."
  [peg subst input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)
        limit (:limit state)
        replacement-fn (if (function? subst)
                         (fn [matched _begin _end]
                           (subst matched))
                         (fn [_matched _begin _end]
                           subst))]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (if (> start limit)
      (do
        (peg-consume-eof! state)
        (:source state))
      (peg-replace-scan compiled
                        state
                        start
                        replacement-fn))))
