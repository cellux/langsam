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
  "Return true when x is a compiled PEG artifact."
  [x]
  (and (map? x)
       (= (get x :kind) :compiled-peg)))

(defn peg-root
  "Choose initial root pattern for compile skeleton."
  [peg-src]
  (if (map? peg-src)
    (if (contains? peg-src :main)
      (get peg-src :main)
      (raise 'peg "grammar map requires :main rule"))
    peg-src))

(defn peg-op-accumulate-alias?
  "Return true when op should behave like accumulate (%)."
  [op]
  (opword? op))

(defn peg-canonical-op
  "Normalize syntactic PEG operator aliases."
  [op]
  (case op
    '+ 'or
    '* 'and
    '> 'look
    '? 'opt
    '<- 'capture
    '/ 'replace
    '$ 'position
    '-> 'backref
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
    (if (peg-op-accumulate-alias? raw-op)
      :accumulate-alias
      (if (integer? raw-op)
        :numeric-repeat
        op))))

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

(defmethod peg-validate-form-op :accumulate-alias
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

(defn compile
  "Compile PEG source into a normalized phase-2 artifact."
  [peg-src]
  (if (compiled-peg? peg-src)
    peg-src
    (let [grammar (if (map? peg-src) peg-src nil)
          rules (if grammar (clone grammar) {})
          compiled {:kind :compiled-peg
                    :source peg-src
                    :grammar grammar
                    :root (peg-root peg-src)
                    :nodes {}
                    :rules rules
                    :defaults (clone peg-default-aliases)}]
      (peg-validate-compiled compiled)
      compiled)))

(defn ensure-compiled
  "Return compiled PEG artifact for peg source or compiled object."
  [peg]
  (if (compiled-peg? peg)
    peg
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

(defn peg-eval-or
  [compiled source (Integer pos) (Integer limit) memo active args]
  (let [matched nil
        it (iter args)]
    (while (and it (nil? matched))
      (let [child @it
            next-pos (peg-eval compiled source pos limit memo active child)]
        (if (some? next-pos)
          (setq matched next-pos)))
      (setq it (next it)))
    matched))

(defn peg-eval-and
  [compiled source (Integer pos) (Integer limit) memo active args]
  (let [cur pos
        ok true
        it (iter args)]
    (while (and it ok)
      (let [child @it
            next-pos (peg-eval compiled source cur limit memo active child)]
        (if (some? next-pos)
          (setq cur next-pos)
          (setq ok false)))
      (setq it (next it)))
    (if ok
      cur
      nil)))

(defn peg-eval-repeat-count
  [compiled source (Integer pos) (Integer limit) memo active (Integer n) child]
  (if (< n 0)
    (raise 'peg "repeat count must be non-negative, got %s" n))
  (let [cur pos
        i 0
        ok true]
    (while (and ok (< i n))
      (let [next (peg-eval compiled source cur limit memo active child)]
        (if (some? next)
          (do
            (setq cur next)
            (incq i))
          (setq ok false))))
    (if ok
      cur
      nil)))

(defn peg-eval-any
  [compiled source (Integer pos) (Integer limit) memo active child]
  (let [cur pos
        looping true]
    (while looping
      (let [next (peg-eval compiled source cur limit memo active child)]
        (if (nil? next)
          (setq looping false)
          (if (= next cur)
            (setq looping false)
            (setq cur next)))))
    cur))

(defn peg-eval-some
  [compiled source (Integer pos) (Integer limit) memo active child]
  (let [first (peg-eval compiled source pos limit memo active child)]
    (if (nil? first)
      nil
      (peg-eval-any compiled source first limit memo active child))))

(defn peg-eval-at-most
  [compiled source (Integer pos) (Integer limit) memo active (Integer n) child]
  (if (< n 0)
    (raise 'peg "at-most count must be non-negative, got %s" n))
  (let [cur pos
        i 0]
    (while (< i n)
      (let [next (peg-eval compiled source cur limit memo active child)]
        (if (nil? next)
          (setq i n)
          (do
            (setq cur next)
            (incq i)))))
    cur))

(defn peg-eval-at-least
  [compiled source (Integer pos) (Integer limit) memo active (Integer n) child]
  (if (< n 0)
    (raise 'peg "at-least count must be non-negative, got %s" n))
  (let [after-min (peg-eval-repeat-count compiled source pos limit memo active n child)]
    (if (nil? after-min)
      nil
      (peg-eval-any compiled source after-min limit memo active child))))

(defn peg-eval-between
  [compiled source (Integer pos) (Integer limit) memo active (Integer min-count) (Integer max-count) child]
  (if (< min-count 0)
    (raise 'peg "between min must be non-negative, got %s" min-count))
  (if (< max-count 0)
    (raise 'peg "between max must be non-negative, got %s" max-count))
  (if (> min-count max-count)
    (raise 'peg "between expects min <= max, got %s > %s" min-count max-count))
  (let [after-min (peg-eval-repeat-count compiled source pos limit memo active min-count child)]
    (if (nil? after-min)
      nil
      (peg-eval-at-most compiled source after-min limit memo active (- max-count min-count) child))))

(defn peg-eval-look
  [compiled source (Integer pos) (Integer limit) memo active args]
  (let [argc (len args)
        argv (Vector args)]
    (if (= argc 1)
      (let [child (get argv 0)
            looked (peg-eval compiled source pos limit memo active child)]
        (if (some? looked)
          pos
          nil))
      (if (= argc 2)
        (let [offset (get argv 0)
              child (get argv 1)]
          (if (not (integer? offset))
            (raise 'peg "look offset should be Integer, got %s" (type offset)))
          (let [target (+ pos offset)]
            (if (< target 0)
              nil
              (let [looked (peg-eval compiled source target limit memo active child)]
                (if (some? looked)
                  pos
                  nil)))))
        (raise 'peg "look expects 1 or 2 args, got %s" argc)))))

(defn peg-eval-opt
  [compiled source (Integer pos) (Integer limit) memo active child]
  (let [next (peg-eval compiled source pos limit memo active child)]
    (if (some? next)
      next
      pos)))

(defn peg-eval-if
  [compiled source (Integer pos) (Integer limit) memo active cond child]
  (let [ok (peg-eval compiled source pos limit memo active cond)]
    (if (some? ok)
      (peg-eval compiled source pos limit memo active child)
      nil)))

(defn peg-eval-if-not
  [compiled source (Integer pos) (Integer limit) memo active cond child]
  (let [ok (peg-eval compiled source pos limit memo active cond)]
    (if (some? ok)
      nil
      (peg-eval compiled source pos limit memo active child))))

(defn peg-eval-not
  [compiled source (Integer pos) (Integer limit) memo active child]
  (let [ok (peg-eval compiled source pos limit memo active child)]
    (if (some? ok)
      nil
      pos)))

(defn peg-eval-form
  [compiled source (Integer pos) (Integer limit) memo active pattern]
  (let [raw-op (car pattern)
        op (peg-canonical-op raw-op)
        args (cdr pattern)
        argc (len args)
        argv (Vector args)]
    (case op
      'or (peg-eval-or compiled source pos limit memo active args)
      'and (peg-eval-and compiled source pos limit memo active args)
      'any (if (= argc 1)
             (peg-eval-any compiled source pos limit memo active (get argv 0))
             (raise 'peg "any expects 1 arg, got %s" argc))
      'some (if (= argc 1)
              (peg-eval-some compiled source pos limit memo active (get argv 0))
              (raise 'peg "some expects 1 arg, got %s" argc))
      'repeat (if (= argc 2)
                (let [n (get argv 0)
                      child (get argv 1)]
                  (if (not (integer? n))
                    (raise 'peg "repeat count should be Integer, got %s" (type n)))
                  (peg-eval-repeat-count compiled source pos limit memo active n child))
                (raise 'peg "repeat expects 2 args, got %s" argc))
      'at-least (if (= argc 2)
                  (let [n (get argv 0)
                        child (get argv 1)]
                    (if (not (integer? n))
                      (raise 'peg "at-least count should be Integer, got %s" (type n)))
                    (peg-eval-at-least compiled source pos limit memo active n child))
                  (raise 'peg "at-least expects 2 args, got %s" argc))
      'at-most (if (= argc 2)
                 (let [n (get argv 0)
                       child (get argv 1)]
                   (if (not (integer? n))
                     (raise 'peg "at-most count should be Integer, got %s" (type n)))
                   (peg-eval-at-most compiled source pos limit memo active n child))
                 (raise 'peg "at-most expects 2 args, got %s" argc))
      'between (if (= argc 3)
                 (let [nmin (get argv 0)
                       nmax (get argv 1)
                       child (get argv 2)]
                   (if (or (not (integer? nmin)) (not (integer? nmax)))
                     (raise 'peg "between counts should be Integer, got %s %s"
                            (type nmin) (type nmax)))
                   (peg-eval-between compiled source pos limit memo active nmin nmax child))
                 (raise 'peg "between expects 3 args, got %s" argc))
      'if (if (= argc 2)
            (peg-eval-if compiled source pos limit memo active (get argv 0) (get argv 1))
            (raise 'peg "if expects 2 args, got %s" argc))
      'if-not (if (= argc 2)
                (peg-eval-if-not compiled source pos limit memo active (get argv 0) (get argv 1))
                (raise 'peg "if-not expects 2 args, got %s" argc))
      'not (if (= argc 1)
             (peg-eval-not compiled source pos limit memo active (get argv 0))
             (raise 'peg "not expects 1 arg, got %s" argc))
      'look (peg-eval-look compiled source pos limit memo active args)
      'opt (if (= argc 1)
             (peg-eval-opt compiled source pos limit memo active (get argv 0))
             (raise 'peg "opt expects 1 arg, got %s" argc))
      'range (peg-match-range source pos limit args)
      'set (if (= argc 1)
             (let [charset (get argv 0)]
               (if (not (string? charset))
                 (raise 'peg "set expects String charset, got %s" (type charset)))
               (peg-match-set source pos limit charset))
             (raise 'peg "set expects 1 arg, got %s" argc))
      (if (integer? raw-op)
        (if (= argc 1)
          (peg-eval-repeat-count compiled source pos limit memo active raw-op (get argv 0))
          (raise 'peg "numeric repeat expects 1 arg, got %s" argc))
        (raise 'peg "unsupported PEG operator: %s" raw-op)))))

(defn peg-eval-reference
  [compiled source (Integer pos) (Integer limit) memo active pattern]
  (let [target (peg-resolve-ref compiled pattern)]
    (if (some? target)
      (peg-eval compiled source pos limit memo active target)
      (raise 'peg "unknown PEG reference: %s" pattern))))

(defmulti peg-eval-dispatch-type
  "Evaluate PEG pattern by runtime type."
  (fn [pattern _compiled _source _pos _limit _memo _active]
    (type pattern)))

(defmethod peg-eval-dispatch-type String
  [pattern _compiled source (Integer pos) (Integer limit) _memo _active]
  (peg-match-string source pos limit pattern))

(defmethod peg-eval-dispatch-type Integer
  [pattern _compiled _source (Integer pos) (Integer limit) _memo _active]
  (peg-match-int pos limit pattern))

(defmethod peg-eval-dispatch-type Boolean
  [pattern _compiled _source (Integer pos) (Integer limit) _memo _active]
  (if pattern
    (if (< pos limit)
      (+ pos 1)
      nil)
    nil))

(defmethod peg-eval-dispatch-type Keyword
  [pattern compiled source (Integer pos) (Integer limit) memo active]
  (peg-eval-reference compiled source pos limit memo active pattern))

(defmethod peg-eval-dispatch-type Symbol
  [pattern compiled source (Integer pos) (Integer limit) memo active]
  (peg-eval-reference compiled source pos limit memo active pattern))

(defmethod peg-eval-dispatch-type Cons
  [pattern compiled source (Integer pos) (Integer limit) memo active]
  (peg-eval-form compiled source pos limit memo active pattern))

(defmethod peg-eval-dispatch-type :default
  [pattern _compiled _source _pos _limit _memo _active]
  (raise 'peg "unsupported PEG pattern type: %s" (type pattern)))

(defn peg-eval-dispatch
  [compiled source (Integer pos) (Integer limit) memo active pattern]
  (peg-eval-dispatch-type pattern compiled source pos limit memo active))

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

(defn peg-form1 [op a]
  (cons op (cons a nil)))

(defn peg-form2 [op a b]
  (cons op (cons a (cons b nil))))

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

(defn peg-eval-iter-width-capture-op
  "Run uint/int-like read op and capture decoded value."
  [source (Integer cur-pos) (Integer limit)
   captures (Integer cap-count) tags
   argc argv op-name read-at]
  (if (or (= argc 1) (= argc 2))
    (let [width (get argv 0)
          tag (if (= argc 2) (get argv 1) nil)]
      (if (not (integer? width))
        (raise 'peg "%s width should be Integer, got %s" op-name (type width)))
      (let [value (read-at source cur-pos limit width)]
        (if (nil? value)
          (peg-eval-iter-return-state captures cap-count tags nil)
          (peg-eval-iter-return-pushed captures
                                       cap-count
                                       tags
                                       value
                                       tag
                                       (+ cur-pos width)))))
    (raise 'peg "%s expects 1 or 2 args, got %s" op-name argc)))

(defn peg-eval-iter-position-op
  "Run position-like capture op that captures current cursor."
  [captures (Integer cap-count) tags (Integer cur-pos) argc argv op-name]
  (if (or (= argc 0) (= argc 1))
    (peg-eval-iter-return-pushed captures
                                 cap-count
                                 tags
                                 cur-pos
                                 (if (= argc 1) (get argv 0) nil)
                                 cur-pos)
    (raise 'peg "%s expects 0 or 1 args, got %s" op-name argc)))

(defn peg-eval-iter-backref-op
  "Run backref-like op and capture latest tag value when present."
  [captures (Integer cap-count) tags (Integer cur-pos) argc argv op-name]
  (if (or (= argc 1) (= argc 2))
    (let [src-tag (get argv 0)
          out-tag (if (= argc 2) (get argv 1) nil)
          vals (get tags src-tag)]
      (if vals
        (peg-eval-iter-return-pushed captures
                                     cap-count
                                     tags
                                     (car vals)
                                     out-tag
                                     cur-pos)
        (peg-eval-iter-return-state captures cap-count tags nil)))
    (raise 'peg "%s expects 1 or 2 args, got %s" op-name argc)))

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
                (cond
                 (string? cur-pattern)
                 (do
                   (setq ret (peg-match-string source cur-pos limit cur-pattern))
                   (setq mode :return))
                 (integer? cur-pattern)
                 (do
                   (setq ret (peg-match-int cur-pos limit cur-pattern))
                   (setq mode :return))
                 (boolean? cur-pattern)
                 (do
                   (setq ret (if cur-pattern
                               (if (< cur-pos limit)
                                 (+ cur-pos 1)
                                 nil)
                               nil))
                   (setq mode :return))
                 (keyword? cur-pattern)
                 (let [target (peg-resolve-ref compiled cur-pattern)]
                   (if (some? target)
                     (setq cur-pattern target)
                     (raise 'peg "unknown PEG reference: %s" cur-pattern)))
                 (symbol? cur-pattern)
                 (let [target (peg-resolve-ref compiled cur-pattern)]
                   (if (some? target)
                     (setq cur-pattern target)
                     (raise 'peg "unknown PEG reference: %s" cur-pattern)))
                 (cons? cur-pattern)
                 (let [op (car cur-pattern)
                       args (cdr cur-pattern)
                       argc (len args)
                       argv (Vector args)]
                   (case op
                     'or
                     (if (= argc 0)
                       (do
                         (setq ret nil)
                         (setq mode :return))
                       (do
                         (setq stack (cons {:kind :or
                                            :args argv
                                            :index 1
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0))))
                     '+
                     (if (= argc 0)
                       (do
                         (setq ret nil)
                         (setq mode :return))
                       (do
                         (setq stack (cons {:kind :or
                                            :args argv
                                            :index 1
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0))))
                     'and
                     (if (= argc 0)
                       (do
                         (setq ret cur-pos)
                         (setq mode :return))
                       (do
                         (setq stack (cons {:kind :and
                                            :args argv
                                            :index 1
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0))))
                     '*
                     (if (= argc 0)
                       (do
                         (setq ret cur-pos)
                         (setq mode :return))
                       (do
                         (setq stack (cons {:kind :and
                                            :args argv
                                            :index 1
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0))))
                     'any
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :any
                                            :child (get argv 0)
                                            :cur cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "any expects 1 arg, got %s" argc))
                     'some
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :some
                                            :child (get argv 0)
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "some expects 1 arg, got %s" argc))
                     'repeat
                     (if (= argc 2)
                       (let [n (get argv 0)
                             child (get argv 1)]
                         (if (not (integer? n))
                           (raise 'peg "repeat count should be Integer, got %s" (type n)))
                         (if (< n 0)
                           (raise 'peg "repeat count must be non-negative, got %s" n))
                         (if (= n 0)
                           (do
                             (setq ret cur-pos)
                             (setq mode :return))
                           (do
                             (setq stack (cons {:kind :repeat
                                                :child child
                                                :remaining n
                                                :start-cap cap-count}
                                               stack))
                             (setq cur-pattern child))))
                       (raise 'peg "repeat expects 2 args, got %s" argc))
                     'at-most
                     (if (= argc 2)
                       (let [n (get argv 0)
                             child (get argv 1)]
                         (if (not (integer? n))
                           (raise 'peg "at-most count should be Integer, got %s" (type n)))
                         (if (< n 0)
                           (raise 'peg "at-most count must be non-negative, got %s" n))
                         (if (= n 0)
                           (do
                             (setq ret cur-pos)
                             (setq mode :return))
                           (do
                             (setq stack (cons {:kind :at-most
                                                :child child
                                                :remaining n
                                                :cur cur-pos
                                                :cap cap-count}
                                               stack))
                             (setq cur-pattern child))))
                       (raise 'peg "at-most expects 2 args, got %s" argc))
                     'at-least
                     (if (= argc 2)
                       (let [n (get argv 0)
                             child (get argv 1)]
                         (if (not (integer? n))
                           (raise 'peg "at-least count should be Integer, got %s" (type n)))
                         (if (< n 0)
                           (raise 'peg "at-least count must be non-negative, got %s" n))
                         (if (= n 0)
                           (do
                             (setq stack (cons {:kind :any
                                                :child child
                                                :cur cur-pos
                                                :cap cap-count}
                                               stack))
                             (setq cur-pattern child))
                           (do
                             (setq stack (cons {:kind :at-least
                                                :child child
                                                :remaining n
                                                :start-cap cap-count}
                                               stack))
                             (setq cur-pattern child))))
                       (raise 'peg "at-least expects 2 args, got %s" argc))
                     'between
                     (if (= argc 3)
                       (let [nmin (get argv 0)
                             nmax (get argv 1)
                             child (get argv 2)]
                         (if (or (not (integer? nmin)) (not (integer? nmax)))
                           (raise 'peg "between counts should be Integer, got %s %s"
                                  (type nmin) (type nmax)))
                         (if (< nmin 0)
                           (raise 'peg "between min must be non-negative, got %s" nmin))
                         (if (< nmax 0)
                           (raise 'peg "between max must be non-negative, got %s" nmax))
                         (if (> nmin nmax)
                           (raise 'peg "between expects min <= max, got %s > %s" nmin nmax))
                         (if (= nmax 0)
                           (do
                             (setq ret cur-pos)
                             (setq mode :return))
                           (if (= nmin 0)
                             (do
                               (setq stack (cons {:kind :at-most
                                                  :child child
                                                  :remaining nmax
                                                  :cur cur-pos
                                                  :cap cap-count}
                                                 stack))
                               (setq cur-pattern child))
                             (do
                               (setq stack (cons {:kind :between
                                                  :child child
                                                  :min-remaining nmin
                                                  :max-remaining nmax
                                                  :start-cap cap-count}
                                                 stack))
                               (setq cur-pattern child)))))
                       (raise 'peg "between expects 3 args, got %s" argc))
                     'if
                     (if (= argc 2)
                       (do
                         (setq stack (cons {:kind :if
                                            :phase :cond
                                            :start cur-pos
                                            :cond-neg false
                                            :child (get argv 1)
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "if expects 2 args, got %s" argc))
                     'if-not
                     (if (= argc 2)
                       (do
                         (setq stack (cons {:kind :if
                                            :phase :cond
                                            :start cur-pos
                                            :cond-neg true
                                            :child (get argv 1)
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "if-not expects 2 args, got %s" argc))
                     'not
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :not
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "not expects 1 arg, got %s" argc))
                     'look
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :look
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (if (= argc 2)
                         (let [offset (get argv 0)
                               child (get argv 1)]
                           (if (not (integer? offset))
                             (raise 'peg "look offset should be Integer, got %s" (type offset)))
                           (let [target (+ cur-pos offset)]
                             (if (< target 0)
                               (do
                                 (setq ret nil)
                                 (setq mode :return))
                               (do
                                 (setq stack (cons {:kind :look
                                                    :start cur-pos
                                                    :cap cap-count}
                                                   stack))
                                 (setq cur-pattern child)
                                 (setq cur-pos target)))))
                         (raise 'peg "look expects 1 or 2 args, got %s" argc)))
                     '>
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :look
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (if (= argc 2)
                         (let [offset (get argv 0)
                               child (get argv 1)]
                           (if (not (integer? offset))
                             (raise 'peg "look offset should be Integer, got %s" (type offset)))
                           (let [target (+ cur-pos offset)]
                             (if (< target 0)
                               (do
                                 (setq ret nil)
                                 (setq mode :return))
                               (do
                                 (setq stack (cons {:kind :look
                                                    :start cur-pos
                                                    :cap cap-count}
                                                   stack))
                                 (setq cur-pattern child)
                                 (setq cur-pos target)))))
                         (raise 'peg "look expects 1 or 2 args, got %s" argc)))
                     'to
                     (if (= argc 1)
                       (let [target (get argv 0)]
                         (setq cur-pattern
                               (peg-form2 'and
                                          (peg-form1 'any
                                                     (peg-form2 'and
                                                                (peg-form1 'not target)
                                                                true))
                                          (peg-form1 'look target))))
                       (raise 'peg "to expects 1 arg, got %s" argc))
                     'thru
                     (if (= argc 1)
                       (let [target (get argv 0)]
                         (setq cur-pattern
                               (peg-form2 'and
                                          (peg-form1 'to target)
                                          target)))
                       (raise 'peg "thru expects 1 arg, got %s" argc))
                     'sub
                     (if (= argc 2)
                       (do
                         (setq stack (cons {:kind :sub
                                            :phase :window
                                            :start cur-pos
                                            :cap cap-count
                                            :limit limit
                                            :child (get argv 1)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "sub expects 2 args, got %s" argc))
                     'split
                     (if (= argc 2)
                       (let [sep (get argv 0)
                             child (get argv 1)]
                         (setq cur-pattern
                               (peg-form1 'opt
                                          (peg-form2 'and
                                                     child
                                                     (peg-form1 'any
                                                                (peg-form2 'and sep child))))))
                       (raise 'peg "split expects 2 args, got %s" argc))
                     'opt
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :opt
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "opt expects 1 arg, got %s" argc))
                     '?
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :opt
                                            :start cur-pos
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "opt expects 1 arg, got %s" argc))
                     'capture
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (let [tag (if (= argc 2) (get argv 1) nil)]
                           (setq stack (cons {:kind :capture
                                              :start cur-pos
                                              :cap cap-count
                                              :tag tag}
                                             stack))
                           (setq cur-pattern (get argv 0))))
                       (raise 'peg "capture expects 1 or 2 args, got %s" argc))
                     '<-
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (let [tag (if (= argc 2) (get argv 1) nil)]
                           (setq stack (cons {:kind :capture
                                              :start cur-pos
                                              :cap cap-count
                                              :tag tag}
                                             stack))
                           (setq cur-pattern (get argv 0))))
                       (raise 'peg "capture expects 1 or 2 args, got %s" argc))
                     'quote
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (let [tag (if (= argc 2) (get argv 1) nil)]
                           (setq stack (cons {:kind :capture
                                              :start cur-pos
                                              :cap cap-count
                                              :tag tag}
                                             stack))
                           (setq cur-pattern (get argv 0))))
                       (raise 'peg "quote expects 1 or 2 args, got %s" argc))
                     'replace
                     (if (or (= argc 2) (= argc 3))
                       (do
                         (setq stack (cons {:kind :replace-cap
                                            :cap cap-count
                                            :subst (get argv 1)
                                            :tag (if (= argc 3) (get argv 2) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "replace expects 2 or 3 args, got %s" argc))
                     '/
                     (if (or (= argc 2) (= argc 3))
                       (do
                         (setq stack (cons {:kind :replace-cap
                                            :cap cap-count
                                            :subst (get argv 1)
                                            :tag (if (= argc 3) (get argv 2) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "replace expects 2 or 3 args, got %s" argc))
                     'group
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (let [tag (if (= argc 2) (get argv 1) nil)]
                           (setq stack (cons {:kind :group
                                              :cap cap-count
                                              :tag tag}
                                             stack))
                           (setq cur-pattern (get argv 0))))
                       (raise 'peg "group expects 1 or 2 args, got %s" argc))
                     'accumulate
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (setq stack (cons {:kind :accumulate
                                            :cap cap-count
                                            :tag (if (= argc 2) (get argv 1) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "accumulate expects 1 or 2 args, got %s" argc))
                     'cmt
                     (if (or (= argc 2) (= argc 3))
                       (do
                         (setq stack (cons {:kind :cmt
                                            :cap cap-count
                                            :f (get argv 1)
                                            :tag (if (= argc 3) (get argv 2) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "cmt expects 2 or 3 args, got %s" argc))
                     'drop
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :drop
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "drop expects 1 arg, got %s" argc))
                     'unref
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (setq stack (cons {:kind :unref
                                            :cap cap-count
                                            :tag (if (= argc 2) (get argv 1) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "unref expects 1 or 2 args, got %s" argc))
                     'only-tags
                     (if (= argc 1)
                       (do
                         (setq stack (cons {:kind :only-tags
                                            :cap cap-count}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "only-tags expects 1 arg, got %s" argc))
                     'nth
                     (if (or (= argc 2) (= argc 3))
                       (let [index (get argv 0)]
                         (if (not (integer? index))
                           (raise 'peg "nth index should be Integer, got %s" (type index)))
                         (setq stack (cons {:kind :nth
                                            :cap cap-count
                                            :index index
                                            :tag (if (= argc 3) (get argv 2) nil)}
                                           stack))
                         (setq cur-pattern (get argv 1)))
                       (raise 'peg "nth expects 2 or 3 args, got %s" argc))
                     'number
                     (if (or (= argc 1) (= argc 2) (= argc 3))
                       (do
                         (setq stack (cons {:kind :number
                                            :cap cap-count
                                            :start cur-pos
                                            :base (if (> argc 1) (get argv 1) nil)
                                            :tag (if (= argc 3) (get argv 2) nil)}
                                           stack))
                         (setq cur-pattern (get argv 0)))
                       (raise 'peg "number expects 1 to 3 args, got %s" argc))
                     'lenprefix
                     (if (= argc 2)
                       (let [width (get argv 0)]
                         (if (not (integer? width))
                           (raise 'peg "lenprefix width should be Integer, got %s" (type width)))
                         (let [window-size (peg-read-uint-le-at source cur-pos limit width)]
                           (if (nil? window-size)
                             (do
                               (setq ret nil)
                               (setq mode :return))
                             (let [start (+ cur-pos width)
                                   end (+ (+ cur-pos width) window-size)]
                               (if (> end limit)
                                 (do
                                   (setq ret nil)
                                   (setq mode :return))
                                 (do
                                   (setq stack (cons {:kind :window
                                                      :cap cap-count
                                                      :limit limit}
                                                     stack))
                                   (setq cur-pattern (get argv 1))
                                   (setq cur-pos start)
                                   (setq limit end)))))))
                       (raise 'peg "lenprefix expects 2 args, got %s" argc))
                     'error
                     (if (or (= argc 0) (= argc 1))
                       (if (= argc 0)
                         (let [[line col] (peg-line-column-at source cur-pos)]
                           (raise 'peg "parse error at line %s column %s" line col))
                         (do
                           (setq stack (cons {:kind :error
                                              :cap cap-count
                                              :start cur-pos}
                                             stack))
                           (setq cur-pattern (get argv 0))))
                       (raise 'peg "error expects 0 or 1 args, got %s" argc))
                     'constant
                     (if (or (= argc 1) (= argc 2))
                       (do
                         (let [state (peg-eval-iter-return-pushed captures
                                                                  cap-count
                                                                  tags
                                                                  (get argv 0)
                                                                  (if (= argc 2) (get argv 1) nil)
                                                                  cur-pos)]
                           (setq captures (:captures state))
                           (setq cap-count (:cap-count state))
                           (setq tags (:tags state))
                           (setq ret (:ret state))
                           (setq mode (:mode state))))
                       (raise 'peg "constant expects 1 or 2 args, got %s" argc))
                     'argument
                     (if (or (= argc 1) (= argc 2))
                       (let [index (get argv 0)
                             out-tag (if (= argc 2) (get argv 1) nil)]
                         (if (not (integer? index))
                           (raise 'peg "argument index should be Integer, got %s" (type index)))
                         (if (< index 0)
                           (raise 'peg "argument index should be non-negative, got %s" index))
                         (let [argcnt (len run-args)
                               value (if (< index argcnt)
                                       (get run-args index)
                                       nil)
                               state (peg-eval-iter-return-pushed captures
                                                                  cap-count
                                                                  tags
                                                                  value
                                                                  out-tag
                                                                  cur-pos)]
                           (setq captures (:captures state))
                           (setq cap-count (:cap-count state))
                           (setq tags (:tags state))
                           (setq ret (:ret state))
                           (setq mode (:mode state))))
                       (raise 'peg "argument expects 1 or 2 args, got %s" argc))
                     'uint
                     (let [state (peg-eval-iter-width-capture-op source
                                                                  cur-pos
                                                                  limit
                                                                  captures
                                                                  cap-count
                                                                  tags
                                                                  argc
                                                                  argv
                                                                  "uint"
                                                                  peg-read-uint-le-at)]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'uint-be
                     (let [state (peg-eval-iter-width-capture-op source
                                                                  cur-pos
                                                                  limit
                                                                  captures
                                                                  cap-count
                                                                  tags
                                                                  argc
                                                                  argv
                                                                  "uint-be"
                                                                  peg-read-uint-be-at)]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'int
                     (let [state (peg-eval-iter-width-capture-op source
                                                                  cur-pos
                                                                  limit
                                                                  captures
                                                                  cap-count
                                                                  tags
                                                                  argc
                                                                  argv
                                                                  "int"
                                                                  peg-read-int-le-at)]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'int-be
                     (let [state (peg-eval-iter-width-capture-op source
                                                                  cur-pos
                                                                  limit
                                                                  captures
                                                                  cap-count
                                                                  tags
                                                                  argc
                                                                  argv
                                                                  "int-be"
                                                                  peg-read-int-be-at)]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'position
                     (let [state (peg-eval-iter-position-op captures
                                                            cap-count
                                                            tags
                                                            cur-pos
                                                            argc
                                                            argv
                                                            "position")]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     '$
                     (let [state (peg-eval-iter-position-op captures
                                                            cap-count
                                                            tags
                                                            cur-pos
                                                            argc
                                                            argv
                                                            "position")]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'line
                     (if (or (= argc 0) (= argc 1))
                       (let [[line _col] (peg-line-column-at source cur-pos)]
                         (let [state (peg-eval-iter-return-pushed captures
                                                                  cap-count
                                                                  tags
                                                                  line
                                                                  (if (= argc 1) (get argv 0) nil)
                                                                  cur-pos)]
                           (setq captures (:captures state))
                           (setq cap-count (:cap-count state))
                           (setq tags (:tags state))
                           (setq ret (:ret state))
                           (setq mode (:mode state))))
                       (raise 'peg "line expects 0 or 1 args, got %s" argc))
                     'column
                     (if (or (= argc 0) (= argc 1))
                       (let [[_line col] (peg-line-column-at source cur-pos)]
                         (let [state (peg-eval-iter-return-pushed captures
                                                                  cap-count
                                                                  tags
                                                                  col
                                                                  (if (= argc 1) (get argv 0) nil)
                                                                  cur-pos)]
                           (setq captures (:captures state))
                           (setq cap-count (:cap-count state))
                           (setq tags (:tags state))
                           (setq ret (:ret state))
                           (setq mode (:mode state))))
                       (raise 'peg "column expects 0 or 1 args, got %s" argc))
                     'backref
                     (let [state (peg-eval-iter-backref-op captures
                                                           cap-count
                                                           tags
                                                           cur-pos
                                                           argc
                                                           argv
                                                           "backref")]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     '->
                     (let [state (peg-eval-iter-backref-op captures
                                                           cap-count
                                                           tags
                                                           cur-pos
                                                           argc
                                                           argv
                                                           "backref")]
                       (setq captures (:captures state))
                       (setq cap-count (:cap-count state))
                       (setq tags (:tags state))
                       (setq ret (:ret state))
                       (setq mode (:mode state)))
                     'backmatch
                     (if (or (= argc 0) (= argc 1))
                       (let [probe (if (= argc 1)
                                     (let [vals (get tags (get argv 0))]
                                       (if vals
                                         (car vals)
                                         nil))
                                     (if (> cap-count 0)
                                       (peg-latest-untagged-capture captures cap-count)
                                       nil))]
                         (if (nil? probe)
                           (do
                             (setq ret nil)
                             (setq mode :return))
                           (let [s (str probe)]
                             (setq ret (peg-match-string source cur-pos limit s))
                             (setq mode :return))))
                       (raise 'peg "backmatch expects 0 or 1 args, got %s" argc))
                     'range
                     (do
                       (setq ret (peg-match-range source cur-pos limit args))
                       (setq mode :return))
                     'set
                     (if (= argc 1)
                       (let [charset (get argv 0)]
                         (if (not (string? charset))
                           (raise 'peg "set expects String charset, got %s" (type charset)))
                         (setq ret (peg-match-set source cur-pos limit charset))
                         (setq mode :return))
                       (raise 'peg "set expects 1 arg, got %s" argc))
                    (if (peg-op-accumulate-alias? op)
                        (if (or (= argc 1) (= argc 2))
                          (do
                            (setq stack (cons {:kind :accumulate
                                               :cap cap-count
                                               :tag (if (= argc 2) (get argv 1) nil)}
                                              stack))
                            (setq cur-pattern (get argv 0)))
                          (raise 'peg "accumulate expects 1 or 2 args, got %s" argc))
                        (if (integer? op)
                          (if (= argc 1)
                            (let [n op
                                  child (get argv 0)]
                              (if (< n 0)
                                (raise 'peg "repeat count must be non-negative, got %s" n))
                              (if (= n 0)
                                (do
                                  (setq ret cur-pos)
                                  (setq mode :return))
                                (do
                                  (setq stack (cons {:kind :repeat
                                                     :child child
                                                     :remaining n
                                                     :start-cap cap-count}
                                                    stack))
                                  (setq cur-pattern child))))
                            (raise 'peg "numeric repeat expects 1 arg, got %s" argc))
                          (raise 'peg "unsupported PEG operator: %s" op)))))
                 (raise 'peg "unsupported PEG pattern type: %s" (type cur-pattern)))))))
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

(defn peg-eval
  [compiled source (Integer pos) (Integer limit) memo active pattern]
  (let [key (peg-memo-key pos pattern)]
    (if (contains? memo key)
      (get memo key)
      (if (contains? active key)
        nil
        (do
          (put active key true)
          (let [result (peg-eval-dispatch compiled source pos limit memo active pattern)]
            (del active key)
            (put memo key result)
            result))))))

(defn peg-resolve
  [compiled state (Integer pos) pattern]
  (let [res (peg-eval-iter compiled
                           (:source state)
                           pos
                           (:limit state)
                           (:memo state)
                           (:active state)
                           pattern
                           (:captures state)
                           (:cap-count state)
                           (:tags state)
                           (:args state)
                           false)]
    (put state :captures (:captures res))
    (put state :cap-count (:cap-count res))
    (put state :tags (:tags res))
    (:end res)))

(defn peg-root-eval-at
  "Evaluate compiled PEG root at source position pos."
  [compiled state (Integer pos)]
  (peg-resolve compiled state pos (:root compiled)))

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
  "Replace PEG matches with substitution; consumes to EOF."
  [peg subst input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)
        limit (:limit state)]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (if (> start limit)
      (do
        (peg-consume-eof! state)
        (:source state))
      (peg-replace-scan compiled
                        state
                        start
                        (fn [_matched _begin _end]
                          subst)))))

(defn replacef
  "Replace PEG matches using function f; consumes to EOF."
  [peg f input &opt ((Integer start) 0) & args]
  (let [compiled (ensure-compiled peg)
        state (peg-make-runstate input args)
        limit (:limit state)]
    (if (< start 0)
      (raise 'peg "start should be non-negative Integer, got %s" start))
    (if (> start limit)
      (do
        (peg-consume-eof! state)
        (:source state))
      (peg-replace-scan compiled
                        state
                        start
                        (fn [matched _begin _end]
                          (f matched))))))
