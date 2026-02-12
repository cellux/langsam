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
        (cond
         (or (string? pattern) (integer? pattern) (boolean? pattern))
         nil
         (keyword? pattern)
         (peg-validate-reference compiled pattern)
         (symbol? pattern)
         (peg-validate-reference compiled pattern)
         (cons? pattern)
         (let [op (car pattern)
               args (cdr pattern)
               argc (len args)
               argv (Vector args)]
           (case op
             'or
             (for [arg args]
               (setq stack (cons arg stack)))
             '+
             (for [arg args]
               (setq stack (cons arg stack)))
             'and
             (for [arg args]
               (setq stack (cons arg stack)))
             '*
             (for [arg args]
               (setq stack (cons arg stack)))
             'any
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "any expects 1 arg, got %s" argc))
             'some
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "some expects 1 arg, got %s" argc))
             'repeat
             (if (= argc 2)
               (let [n (get argv 0)]
                 (if (not (integer? n))
                   (raise 'peg "repeat count should be Integer, got %s" (type n)))
                 (if (< n 0)
                   (raise 'peg "repeat count must be non-negative, got %s" n))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "repeat expects 2 args, got %s" argc))
             'at-least
             (if (= argc 2)
               (let [n (get argv 0)]
                 (if (not (integer? n))
                   (raise 'peg "at-least count should be Integer, got %s" (type n)))
                 (if (< n 0)
                   (raise 'peg "at-least count must be non-negative, got %s" n))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "at-least expects 2 args, got %s" argc))
             'at-most
             (if (= argc 2)
               (let [n (get argv 0)]
                 (if (not (integer? n))
                   (raise 'peg "at-most count should be Integer, got %s" (type n)))
                 (if (< n 0)
                   (raise 'peg "at-most count must be non-negative, got %s" n))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "at-most expects 2 args, got %s" argc))
             'between
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
                 (setq stack (cons (get argv 2) stack)))
               (raise 'peg "between expects 3 args, got %s" argc))
             'if
             (if (= argc 2)
               (do
                 (setq stack (cons (get argv 0) stack))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "if expects 2 args, got %s" argc))
             'if-not
             (if (= argc 2)
               (do
                 (setq stack (cons (get argv 0) stack))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "if-not expects 2 args, got %s" argc))
             'not
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "not expects 1 arg, got %s" argc))
             'look
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (if (= argc 2)
                 (let [offset (get argv 0)]
                   (if (not (integer? offset))
                     (raise 'peg "look offset should be Integer, got %s" (type offset)))
                   (setq stack (cons (get argv 1) stack)))
                 (raise 'peg "look expects 1 or 2 args, got %s" argc)))
             '>
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (if (= argc 2)
                 (let [offset (get argv 0)]
                   (if (not (integer? offset))
                     (raise 'peg "look offset should be Integer, got %s" (type offset)))
                   (setq stack (cons (get argv 1) stack)))
                 (raise 'peg "look expects 1 or 2 args, got %s" argc)))
             'to
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "to expects 1 arg, got %s" argc))
             'thru
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "thru expects 1 arg, got %s" argc))
             'sub
             (if (= argc 2)
               (do
                 (setq stack (cons (get argv 0) stack))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "sub expects 2 args, got %s" argc))
             'split
             (if (= argc 2)
               (do
                 (setq stack (cons (get argv 0) stack))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "split expects 2 args, got %s" argc))
             'opt
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "opt expects 1 arg, got %s" argc))
             '?
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "opt expects 1 arg, got %s" argc))
             'capture
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "capture expects 1 or 2 args, got %s" argc))
             '<-
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "capture expects 1 or 2 args, got %s" argc))
             'quote
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "quote expects 1 or 2 args, got %s" argc))
             'replace
             (if (or (= argc 2) (= argc 3))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "replace expects 2 or 3 args, got %s" argc))
             '/
             (if (or (= argc 2) (= argc 3))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "replace expects 2 or 3 args, got %s" argc))
             'group
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "group expects 1 or 2 args, got %s" argc))
             'accumulate
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "accumulate expects 1 or 2 args, got %s" argc))
             'cmt
             (if (or (= argc 2) (= argc 3))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "cmt expects 2 or 3 args, got %s" argc))
             'drop
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "drop expects 1 arg, got %s" argc))
             'unref
             (if (or (= argc 1) (= argc 2))
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "unref expects 1 or 2 args, got %s" argc))
             'only-tags
             (if (= argc 1)
               (setq stack (cons (get argv 0) stack))
               (raise 'peg "only-tags expects 1 arg, got %s" argc))
             'nth
             (if (or (= argc 2) (= argc 3))
               (let [index (get argv 0)]
                 (if (not (integer? index))
                   (raise 'peg "nth index should be Integer, got %s" (type index)))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "nth expects 2 or 3 args, got %s" argc))
             'number
             (if (or (= argc 1) (= argc 2) (= argc 3))
               (do
                 (if (> argc 1)
                   (let [base (get argv 1)]
                     (if (and (some? base) (not (integer? base)))
                       (raise 'peg "number base should be Integer, got %s" (type base)))))
                 (setq stack (cons (get argv 0) stack)))
               (raise 'peg "number expects 1 to 3 args, got %s" argc))
             'lenprefix
             (if (= argc 2)
               (let [width (get argv 0)]
                 (if (not (integer? width))
                   (raise 'peg "lenprefix width should be Integer, got %s" (type width)))
                 (if (<= width 0)
                   (raise 'peg "integer width should be positive, got %s" width))
                 (setq stack (cons (get argv 1) stack)))
               (raise 'peg "lenprefix expects 2 args, got %s" argc))
             'error
             (if (or (= argc 0) (= argc 1))
               (if (= argc 1)
                 (setq stack (cons (get argv 0) stack)))
               (raise 'peg "error expects 0 or 1 args, got %s" argc))
             'constant
             (if (or (= argc 1) (= argc 2))
               nil
               (raise 'peg "constant expects 1 or 2 args, got %s" argc))
             'argument
             (if (or (= argc 1) (= argc 2))
               (let [index (get argv 0)]
                 (if (not (integer? index))
                   (raise 'peg "argument index should be Integer, got %s" (type index)))
                 (if (< index 0)
                   (raise 'peg "argument index should be non-negative, got %s" index)))
               (raise 'peg "argument expects 1 or 2 args, got %s" argc))
             'uint
             (if (or (= argc 1) (= argc 2))
               (let [width (get argv 0)]
                 (if (not (integer? width))
                   (raise 'peg "uint width should be Integer, got %s" (type width)))
                 (if (<= width 0)
                   (raise 'peg "integer width should be positive, got %s" width)))
               (raise 'peg "uint expects 1 or 2 args, got %s" argc))
             'uint-be
             (if (or (= argc 1) (= argc 2))
               (let [width (get argv 0)]
                 (if (not (integer? width))
                   (raise 'peg "uint-be width should be Integer, got %s" (type width)))
                 (if (<= width 0)
                   (raise 'peg "integer width should be positive, got %s" width)))
               (raise 'peg "uint-be expects 1 or 2 args, got %s" argc))
             'int
             (if (or (= argc 1) (= argc 2))
               (let [width (get argv 0)]
                 (if (not (integer? width))
                   (raise 'peg "int width should be Integer, got %s" (type width)))
                 (if (<= width 0)
                   (raise 'peg "integer width should be positive, got %s" width)))
               (raise 'peg "int expects 1 or 2 args, got %s" argc))
             'int-be
             (if (or (= argc 1) (= argc 2))
               (let [width (get argv 0)]
                 (if (not (integer? width))
                   (raise 'peg "int-be width should be Integer, got %s" (type width)))
                 (if (<= width 0)
                   (raise 'peg "integer width should be positive, got %s" width)))
               (raise 'peg "int-be expects 1 or 2 args, got %s" argc))
             'position
             (if (or (= argc 0) (= argc 1))
               nil
               (raise 'peg "position expects 0 or 1 args, got %s" argc))
             '$
             (if (or (= argc 0) (= argc 1))
               nil
               (raise 'peg "position expects 0 or 1 args, got %s" argc))
             'line
             (if (or (= argc 0) (= argc 1))
               nil
               (raise 'peg "line expects 0 or 1 args, got %s" argc))
             'column
             (if (or (= argc 0) (= argc 1))
               nil
               (raise 'peg "column expects 0 or 1 args, got %s" argc))
             'backref
             (if (or (= argc 1) (= argc 2))
               nil
               (raise 'peg "backref expects 1 or 2 args, got %s" argc))
             '->
             (if (or (= argc 1) (= argc 2))
               nil
               (raise 'peg "backref expects 1 or 2 args, got %s" argc))
             'backmatch
             (if (or (= argc 0) (= argc 1))
               nil
               (raise 'peg "backmatch expects 0 or 1 args, got %s" argc))
             'range
             (peg-validate-range-entries args)
             'set
             (if (= argc 1)
               (let [charset (get argv 0)]
                 (if (not (string? charset))
                   (raise 'peg "set expects String charset, got %s" (type charset))))
               (raise 'peg "set expects 1 arg, got %s" argc))
             (if (peg-op-accumulate-alias? op)
               (if (or (= argc 1) (= argc 2))
                 (setq stack (cons (get argv 0) stack))
                 (raise 'peg "accumulate expects 1 or 2 args, got %s" argc))
               (if (integer? op)
                 (if (= argc 1)
                   (do
                     (if (< op 0)
                       (raise 'peg "repeat count must be non-negative, got %s" op))
                     (setq stack (cons (get argv 0) stack)))
                   (raise 'peg "numeric repeat expects 1 arg, got %s" argc))
                 (raise 'peg "unsupported PEG operator: %s" op)))))
         (raise 'peg "unsupported PEG pattern type: %s" (type pattern)))))
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

(defn peg-eval-choice
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

(defn peg-eval-sequence
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
  (let [op (car pattern)
        args (cdr pattern)
        argc (len args)
        argv (Vector args)]
    (case op
      'or (peg-eval-choice compiled source pos limit memo active args)
      '+ (peg-eval-choice compiled source pos limit memo active args)
      'and (peg-eval-sequence compiled source pos limit memo active args)
      '* (peg-eval-sequence compiled source pos limit memo active args)
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
      '> (peg-eval-look compiled source pos limit memo active args)
      'opt (if (= argc 1)
             (peg-eval-opt compiled source pos limit memo active (get argv 0))
             (raise 'peg "opt expects 1 arg, got %s" argc))
      '? (if (= argc 1)
           (peg-eval-opt compiled source pos limit memo active (get argv 0))
           (raise 'peg "opt expects 1 arg, got %s" argc))
      'range (peg-match-range source pos limit args)
      'set (if (= argc 1)
             (let [charset (get argv 0)]
               (if (not (string? charset))
                 (raise 'peg "set expects String charset, got %s" (type charset)))
               (peg-match-set source pos limit charset))
             (raise 'peg "set expects 1 arg, got %s" argc))
      (if (integer? op)
        (if (= argc 1)
          (peg-eval-repeat-count compiled source pos limit memo active op (get argv 0))
          (raise 'peg "numeric repeat expects 1 arg, got %s" argc))
        (raise 'peg "unsupported PEG operator: %s" op)))))

(defn peg-eval-dispatch
  [compiled source (Integer pos) (Integer limit) memo active pattern]
  (cond
   (string? pattern)
   (peg-match-string source pos limit pattern)
   (integer? pattern)
   (peg-match-int pos limit pattern)
   (boolean? pattern)
   (if pattern
     (if (< pos limit)
       (+ pos 1)
       nil)
     nil)
   (keyword? pattern)
   (let [target (peg-resolve-ref compiled pattern)]
     (if (some? target)
       (peg-eval compiled source pos limit memo active target)
       (raise 'peg "unknown PEG reference: %s" pattern)))
   (symbol? pattern)
   (let [target (peg-resolve-ref compiled pattern)]
     (if (some? target)
       (peg-eval compiled source pos limit memo active target)
       (raise 'peg "unknown PEG reference: %s" pattern)))
   (cons? pattern)
   (peg-eval-form compiled source pos limit memo active pattern)
   (raise 'peg "unsupported PEG pattern type: %s" (type pattern))))

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
                         (setq stack (cons {:kind :choice
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
                         (setq stack (cons {:kind :choice
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
                         (setq stack (cons {:kind :sequence
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
                         (setq stack (cons {:kind :sequence
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
                         (let [[caps n t] (peg-capture-push captures
                                                            cap-count
                                                            tags
                                                            (get argv 0)
                                                            (if (= argc 2) (get argv 1) nil))]
                           (setq captures caps)
                           (setq cap-count n)
                           (setq tags t))
                         (setq ret cur-pos)
                         (setq mode :return))
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
                               pushed (peg-capture-push captures cap-count tags value out-tag)]
                           (setq captures (get pushed 0))
                           (setq cap-count (get pushed 1))
                           (setq tags (get pushed 2)))
                         (setq ret cur-pos)
                         (setq mode :return))
                       (raise 'peg "argument expects 1 or 2 args, got %s" argc))
                     'uint
                     (if (or (= argc 1) (= argc 2))
                       (let [width (get argv 0)
                             tag (if (= argc 2) (get argv 1) nil)]
                         (if (not (integer? width))
                           (raise 'peg "uint width should be Integer, got %s" (type width)))
                         (let [value (peg-read-uint-le-at source cur-pos limit width)]
                           (if (nil? value)
                             (do
                               (setq ret nil)
                               (setq mode :return))
                             (do
                               (let [pushed (peg-capture-push captures cap-count tags value tag)]
                                 (setq captures (get pushed 0))
                                 (setq cap-count (get pushed 1))
                                 (setq tags (get pushed 2)))
                               (setq ret (+ cur-pos width))
                               (setq mode :return)))))
                       (raise 'peg "uint expects 1 or 2 args, got %s" argc))
                     'uint-be
                     (if (or (= argc 1) (= argc 2))
                       (let [width (get argv 0)
                             tag (if (= argc 2) (get argv 1) nil)]
                         (if (not (integer? width))
                           (raise 'peg "uint-be width should be Integer, got %s" (type width)))
                         (let [value (peg-read-uint-be-at source cur-pos limit width)]
                           (if (nil? value)
                             (do
                               (setq ret nil)
                               (setq mode :return))
                             (do
                               (let [pushed (peg-capture-push captures cap-count tags value tag)]
                                 (setq captures (get pushed 0))
                                 (setq cap-count (get pushed 1))
                                 (setq tags (get pushed 2)))
                               (setq ret (+ cur-pos width))
                               (setq mode :return)))))
                       (raise 'peg "uint-be expects 1 or 2 args, got %s" argc))
                     'int
                     (if (or (= argc 1) (= argc 2))
                       (let [width (get argv 0)
                             tag (if (= argc 2) (get argv 1) nil)]
                         (if (not (integer? width))
                           (raise 'peg "int width should be Integer, got %s" (type width)))
                         (let [value (peg-read-int-le-at source cur-pos limit width)]
                           (if (nil? value)
                             (do
                               (setq ret nil)
                               (setq mode :return))
                             (do
                               (let [pushed (peg-capture-push captures cap-count tags value tag)]
                                 (setq captures (get pushed 0))
                                 (setq cap-count (get pushed 1))
                                 (setq tags (get pushed 2)))
                               (setq ret (+ cur-pos width))
                               (setq mode :return)))))
                       (raise 'peg "int expects 1 or 2 args, got %s" argc))
                     'int-be
                     (if (or (= argc 1) (= argc 2))
                       (let [width (get argv 0)
                             tag (if (= argc 2) (get argv 1) nil)]
                         (if (not (integer? width))
                           (raise 'peg "int-be width should be Integer, got %s" (type width)))
                         (let [value (peg-read-int-be-at source cur-pos limit width)]
                           (if (nil? value)
                             (do
                               (setq ret nil)
                               (setq mode :return))
                             (do
                               (let [pushed (peg-capture-push captures cap-count tags value tag)]
                                 (setq captures (get pushed 0))
                                 (setq cap-count (get pushed 1))
                                 (setq tags (get pushed 2)))
                               (setq ret (+ cur-pos width))
                               (setq mode :return)))))
                       (raise 'peg "int-be expects 1 or 2 args, got %s" argc))
                     'position
                     (if (or (= argc 0) (= argc 1))
                       (do
                         (let [[caps n t] (peg-capture-push captures
                                                            cap-count
                                                            tags
                                                            cur-pos
                                                            (if (= argc 1) (get argv 0) nil))]
                           (setq captures caps)
                           (setq cap-count n)
                           (setq tags t))
                         (setq ret cur-pos)
                         (setq mode :return))
                       (raise 'peg "position expects 0 or 1 args, got %s" argc))
                     '$
                     (if (or (= argc 0) (= argc 1))
                       (do
                         (let [[caps n t] (peg-capture-push captures
                                                            cap-count
                                                            tags
                                                            cur-pos
                                                            (if (= argc 1) (get argv 0) nil))]
                           (setq captures caps)
                           (setq cap-count n)
                           (setq tags t))
                         (setq ret cur-pos)
                         (setq mode :return))
                       (raise 'peg "position expects 0 or 1 args, got %s" argc))
                     'line
                     (if (or (= argc 0) (= argc 1))
                       (let [[line _col] (peg-line-column-at source cur-pos)]
                         (let [[caps n t] (peg-capture-push captures
                                                            cap-count
                                                            tags
                                                            line
                                                            (if (= argc 1) (get argv 0) nil))]
                           (setq captures caps)
                           (setq cap-count n)
                           (setq tags t))
                         (setq ret cur-pos)
                         (setq mode :return))
                       (raise 'peg "line expects 0 or 1 args, got %s" argc))
                     'column
                     (if (or (= argc 0) (= argc 1))
                       (let [[_line col] (peg-line-column-at source cur-pos)]
                         (let [[caps n t] (peg-capture-push captures
                                                            cap-count
                                                            tags
                                                            col
                                                            (if (= argc 1) (get argv 0) nil))]
                           (setq captures caps)
                           (setq cap-count n)
                           (setq tags t))
                         (setq ret cur-pos)
                         (setq mode :return))
                       (raise 'peg "column expects 0 or 1 args, got %s" argc))
                     'backref
                     (if (or (= argc 1) (= argc 2))
                       (let [src-tag (get argv 0)
                             out-tag (if (= argc 2) (get argv 1) nil)
                             vals (get tags src-tag)]
                         (if vals
                           (do
                             (let [v (car vals)
                                   pushed (peg-capture-push captures cap-count tags v out-tag)]
                               (setq captures (get pushed 0))
                               (setq cap-count (get pushed 1))
                               (setq tags (get pushed 2)))
                             (setq ret cur-pos)
                             (setq mode :return))
                           (do
                             (setq ret nil)
                             (setq mode :return))))
                       (raise 'peg "backref expects 1 or 2 args, got %s" argc))
                     '->
                     (if (or (= argc 1) (= argc 2))
                       (let [src-tag (get argv 0)
                             out-tag (if (= argc 2) (get argv 1) nil)
                             vals (get tags src-tag)]
                         (if vals
                           (do
                             (let [v (car vals)
                                   pushed (peg-capture-push captures cap-count tags v out-tag)]
                               (setq captures (get pushed 0))
                               (setq cap-count (get pushed 1))
                               (setq tags (get pushed 2)))
                             (setq ret cur-pos)
                             (setq mode :return))
                           (do
                             (setq ret nil)
                             (setq mode :return))))
                       (raise 'peg "backref expects 1 or 2 args, got %s" argc))
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
              :choice
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
                      (setq stack (cons {:kind :choice
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
              :sequence
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
                      (setq stack (cons {:kind :sequence
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
