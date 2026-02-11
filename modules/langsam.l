; ------------------------------
; Predicates and Core Utilities
; ------------------------------

(defn type?
  "Return true when x is a Type."
  [x]
  (= (type x) Type))

(defn nil?
  "Return true when x is nil."
  [x]
  (= x nil))

(defn some?
  "Return true when x is not nil."
  [x]
  (not= x nil))

(defn boolean?
  "Return true when x is a Boolean."
  [x]
  (= (type x) Boolean))

(defn integer?
  "Return true when x is an Integer."
  [x]
  (= (type x) Integer))

(defn float?
  "Return true when x is a Float."
  [x]
  (= (type x) Float))

(defn string?
  "Return true when x is a String."
  [x]
  (= (type x) String))

(defn symbol?
  "Return true when x is a Symbol."
  [x]
  (= (type x) Symbol))

(defn keyword?
  "Return true when x is a Keyword."
  [x]
  (= (type x) Keyword))

(defn opword?
  "Return true when x is an Opword."
  [x]
  (= (type x) Opword))

(defn cons?
  "Return true when x is a Cons."
  [x]
  (= (type x) Cons))

(defn proper-list?
  "Return true when x is a nil-terminated Cons list."
  [x]
  (if (nil? x)
    true
    (if (cons? x)
      (proper-list? (cdr x))
      false)))

(defn vector?
  "Return true when x is a Vector."
  [x]
  (= (type x) Vector))

(defn map?
  "Return true when x is a Map."
  [x]
  (= (type x) Map))

(defn function?
  "Return true when x is a Function."
  [x]
  (= (type x) Function))

(defn raise
  "Raise an exception payload built from kind and formatted message."
  [kind fmt & args]
  (throw (cons kind (apply format fmt args))))

(defn not
  "Return logical negation of x."
  [x]
  (if x false true))

(defn not=
  "Return true when x and rest are not all equal."
  [x & rest]
  (not (apply = x rest)))

(defn min
  "Return the smallest argument."
  [& args]
  (if-match [[x & rest] args]
    (let [result x
          it (iter rest)]
      (while it
        (let [value @it]
          (if (< value result)
            (setq result value)))
        (setq it (next it)))
      result)
    (raise 'min "min expects at least one argument")))

(defn max
  "Return the greatest argument."
  [& args]
  (if-match [[x & rest] args]
    (let [result x
          it (iter rest)]
      (while it
        (let [value @it]
          (if (> value result)
            (setq result value)))
        (setq it (next it)))
      result)
    (raise 'max "max expects at least one argument")))

(defn special?
  "Return true when x is a special form function."
  [x]
  (and (function? x)
       (not (:evalargs x))
       (not (:evalresult x))))

(defmacro when
  "Evaluate body when cond is truthy."
  [cond & body]
  `(if ,cond (do ,@body)))

(defmacro unless
  "Evaluate body when cond is falsey."
  [cond & body]
  `(if (not ,cond) (do ,@body)))

(def cond
  (let [subcond (fn [clauses]
                  (if clauses
                    (if-match [[test then & rest] clauses]
                      `(if ,test ,then ,(subcond rest))
                      (let [[fallback] clauses]
                        fallback))
                    nil))]
    (macro cond
      "Evaluate clauses in order and return the first matching result."
      [& clauses]
      (subcond clauses))))

(defn sublet
  "Create a child map with proto as prototype."
  [proto &opt (size 64)]
  (let [sublet (Map size)]
    (setproto sublet proto)
    sublet))

(defmacro import
  "Bind a module map or selected symbols from a module."
  [name & opts]
  (if (nil? opts)
    `(def ,name (require ,(str name)))
    (if-match [[:refer refs] opts]
      (do
        (if (not (vector? refs))
          (raise 'import "import :refer expects Vector, got %s" (type refs)))
        `(do
           ,@(map (fn [sym]
                    (if (not (symbol? sym))
                      (raise 'import
                             "import :refer entries should be Symbol, got %s"
                             (type sym)))
                    `(def ,sym (get (require ,(str name)) ',sym)))
                  refs))))
      (raise 'import
             "import supports (import name) or (import name :refer [symbols]), got %s"
             (cons name opts))))

(defn update
  "Update key k in x by applying f to the existing value."
  [x k f & rest]
  (let [oldv (get x k)
        newv (apply f oldv rest)]
    (put x k newv)))

(defmacro comment
  "Ignore body forms."
  [& body])

(def reset
  setcdr
  "Set ref cdr to value and return nil.")

(defn swap
  "Update ref by applying f to its current value and return the new value."
  [ref f & rest]
  (let [newv (apply f @ref rest)]
    (reset ref newv)
    newv))

(defmacro setq
  "Set an existing lexical binding."
  [x val]
  `(reset (gep (curlet) ',x) ,val))

(defmacro swapq
  "Update binding x by applying f."
  [x f & rest]
  `(swap (gep (curlet) ',x) ,f ,@rest))

(defn inc
  "Return x + 1."
  [x]
  (+ x 1))

(defmacro incq
  "Increment binding x in place."
  [x]
  `(swapq ,x ,inc))

(defn dec
  "Return x - 1."
  [x]
  (- x 1))

(defmacro decq
  "Decrement binding x in place."
  [x]
  `(swapq ,x ,dec))

; -------------------
; Numeric Predicates
; -------------------

(defn even?
  "Return true when integer x is even."
  [x]
  (= (mod x 2) 0))

(defn odd?
  "Return true when integer x is odd."
  [x]
  (not= (mod x 2) 0))

(defn zero?
  "Return true when numeric x equals zero."
  [x]
  (cond-match x
    (Integer i) (= i 0)
    (Float f) (= f 0.0)
    false))

(defn nonzero?
  "Return true when numeric x is not zero."
  [x]
  (not (zero? x)))

(defn pos?
  "Return true when numeric x is positive."
  [x]
  (cond-match x
    (Integer i) (> i 0)
    (Float f) (> f 0.0)
    false))

(defn neg?
  "Return true when numeric x is negative."
  [x]
  (cond-match x
    (Integer i) (< i 0)
    (Float f) (< f 0.0)
    false))

; ---------------------------
; Sequence and Data Functions
; ---------------------------

(defn map
  "Map f over coll and return a list."
  [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (setq result (cons (f @it) result))
      (setq it (next it)))
    (nreverse result)))

(defn mapv
  "Map f over coll and return a Vector."
  [f coll]
  (Vector (map f coll)))

(defn mapcat
  "Map f over coll and concatenate results one level into a list."
  [f coll]
  (flatten1 (map f coll)))

(defn reduce
  "Fold coll from left with accumulator init and function f."
  [f init coll]
  (let [result init
        it (iter coll)]
    (while it
      (setq result (f @it result))
      (setq it (next it)))
    result))

(defn filter
  "Return a list of elements in coll where f is truthy."
  [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (let [item @it]
        (if (f item)
          (setq result (cons item result))))
      (setq it (next it)))
    (nreverse result)))

(defn drop
  "Drop the first n elements from coll."
  [n coll]
  (cond
   (nil? coll) nil
   (<= n 0) coll
   (cons? coll) (let [result coll
                      i 0]
                  (while (and (cons? result) (< i n))
                    (setq result (cdr result))
                    (incq i))
                  (if (cons? result) result nil))
   (let [it (iter coll)
         i 0
         result nil]
     (while (and it (< i n))
       (setq it (next it))
       (incq i))
     (while it
       (setq result (cons @it result))
       (setq it (next it)))
     ((type coll) (nreverse result)))))

(defn tail
  "Return coll without its first element."
  [coll]
  (drop 1 coll))

(defn -range-empty?
  [start end step]
  (cond
   (pos? step) (>= start end)
   (<= start end)))

(defn -range-iterator
  [start end step]
  (cond
   (or (not (integer? start))
       (not (integer? end))
       (not (integer? step)))
   (raise 'range "range expects Integer arguments, got %s %s %s"
          (type start) (type end) (type step))

   (= step 0)
   (raise 'range "range step cannot be 0")

   (-range-empty? start end step)
   nil

   (let [it {:i start
             :end end
             :step step
             :consumed false}]
     (put it %iter (fn [self] self))
     (put it %deref (fn [self]
                      (if (:consumed self)
                        (raise 'deref "attempt to deref consumed RangeIterator")
                        (:i self))))
     (put it %invoke (fn [self]
                       (if (:consumed self)
                         (raise 'invoke "attempt to advance consumed RangeIterator")
                         (let [next-i (+ (:i self) (:step self))]
                           (if (-range-empty? next-i (:end self) (:step self))
                             (do
                               (put self :consumed true)
                               nil)
                             (do
                               (put self :i next-i)
                               self))))))
     it)))

(defn range
  "Return an iterator over an integer range."
  [& args]
  (let [n (len args)]
    (cond
     (= n 1)
     (let [[end] args]
       (-range-iterator 0 end 1))

     (= n 2)
     (let [[start end] args]
       (-range-iterator start end 1))

     (= n 3)
     (let [[start end step] args]
       (-range-iterator start end step))

     (raise 'range "range expects 1 to 3 args, got %s" n))))

(defn member?
  "Return true when x is equal to an element in coll."
  [x coll]
  (let [it (iter coll)
        found false]
    (while (and it (not found))
      (if (= x @it)
        (setq found true)
        (setq it (next it))))
    found))

(defn every?
  "Return true when pred returns truthy for all elements."
  [pred coll]
  (let [it (iter coll)
        ok true]
    (while (and it ok)
      (if (not (pred @it))
        (setq ok false))
      (setq it (next it)))
    ok))

(defn any?
  "Return true when pred returns truthy for any element."
  [pred coll]
  (let [it (iter coll)
        ok false]
    (while (and it (not ok))
      (if (pred @it)
        (setq ok true))
      (setq it (next it)))
    ok))

; ---------------------------------
; Logical and Binding Control Forms
; ---------------------------------

(def gensym
  (let [counter 0]
    (fn gensym
      "Return a fresh symbol using prefix."
      [&opt (prefix 'gensym)]
      (let [sym (Symbol (str prefix "#" counter))]
        (incq counter)
        sym))))

(defmacro and
  "Return first falsey form, otherwise the last form."
  [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# (and ,@rest) ,val#)))
    x))

(defmacro or
  "Return first truthy form, otherwise the last falsey form."
  [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# ,val# (or ,@rest))))
    x))

(defmacro when-let
  "Evaluate body when all if-let bindings are truthy."
  [bindings & body]
  `(if-let ,bindings (do ,@body)))

(defmacro while-let
  "Loop while if-let bindings continue to match truthy values."
  [bindings & body]
  `(while (if-let ,bindings (do ,@body true) false)))

(defmacro when-match
  "Evaluate body when if-match bindings match."
  [bindings & body]
  `(if-match ,bindings (do ,@body)))

(defmacro while-match
  "Loop while if-match bindings continue to match."
  [bindings & body]
  `(while (if-match ,bindings (do ,@body true) false)))

(def cond-let
  (let [subcond (fn [clauses]
                  (if clauses
                    (if-match [[bindings result & clauses] clauses]
                      `(if-let ,bindings
                         ,result
                         ,(subcond clauses))
                      (let [[result] clauses]
                        result))
                    `(raise 'cond-let "no match")))]
    (macro cond-let [& clauses]
      "Evaluate clauses in order, returning first truthy-binding match."
      (subcond clauses))))

(def cond-match
  (let [subcond (fn [item# clauses]
                  (if clauses
                    (if-match [[pattern result & clauses] clauses]
                      `(if-match [,pattern ,item#]
                         ,result
                         ,(subcond item# clauses))
                      (let [[result] clauses]
                        result))
                    `(raise 'cond-match "no match")))]
    (macro cond-match [item & clauses]
      "Evaluate clauses in order, returning first pattern match result."
      (let [item# (gensym 'item)]
        `(let [,item# ,item]
           ,(subcond item# clauses))))))

(def case
  (let [subcase (fn [item# clauses]
                  (if clauses
                    (if-match [[pattern result & clauses] clauses]
                      `(if-match [,pattern ,item#]
                         ,result
                         ,(subcase item# clauses))
                      (let [[result] clauses]
                        result))
                    `(raise 'case "no match")))]
    (macro case [item & clauses]
      "Evaluate item once and return the first matching pattern result."
      (let [item# (gensym 'item)]
        `(let [,item# ,item]
           ,(subcase item# clauses))))))

(defmacro prog1
  "Evaluate head, then rest, and return head's value."
  [head & rest]
  (let [result# (gensym 'result)]
    `(do (def ,result# ,head)
         ,@rest
         ,result#)))

(defmacro for
  "Iterate over binding pairs and evaluate body."
  [bindings & body]
  (if-match [[pat coll & rest] bindings]
    (let [it# (gensym 'it)]
      `(let [,it# (iter ,coll)]
         (while ,it#
           (let [,pat @,it#]
             (for ,rest ,@body)
             (setq ,it# (next ,it#))))))
    `(do ,@body)))

; ------------------------------
; Collection Transform Utilities
; ------------------------------

(defn group-by
  "Group coll by keyfn into a map of lists."
  [keyfn coll]
  (let [groups {}
        it (iter coll)]
    (while it
      (let [item @it
            key (keyfn item)]
        (update groups key (fn [group item] (cons item group)) item))
      (setq it (next it)))
    groups))

(defn separate
  "Return [items-where-pred-true items-where-pred-false]."
  [pred coll]
  (let [left nil
        right nil
        it (iter coll)]
    (while it
      (let [item @it]
        (if (pred item)
          (setq left (cons item left))
          (setq right (cons item right))))
      (setq it (next it)))
    [(nreverse left) (nreverse right)]))

(defn partition
  "Partition coll into vectors of up to n items."
  [n coll]
  (let [groups nil
        it (iter coll)]
    (while it
      (let [group nil
            remaining n]
        (while (and it (> remaining 0))
          (setq group (cons @it group))
          (setq it (next it))
          (decq remaining))
        (setq groups (cons (Vector (nreverse group)) groups))))
    (nreverse groups)))

(defn comp
  "Compose f after g."
  [f g]
  (fn [& rest] (f (apply g rest))))

(defn iterable?
  "Return true when x supports iter."
  [x]
  (catch [_ false]
    (iter x)
    true))

(defn catv
  "Concatenate parts into a Vector, flattening iterables one level."
  [& parts]
  (let [result nil]
    (for [part parts]
      (if (iterable? part)
        (for [x part]
          (setq result (cons x result)))
        (setq result (cons part result))))
    (Vector (nreverse result))))

(defn flatten
  "Recursively flatten nested iterables preserving outer type."
  [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x (flatten item)]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defn flatten1
  "Flatten only the first nesting level preserving outer type."
  [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x item]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

; ------------------------
; Exception Helper Macros
; ------------------------

(defmacro throws?
  "Assert body throws an exception with expected payload."
  [expected-payload & body]
  (let [catch-result# (gensym 'catch-result)]
    `(let [,catch-result# (catch [payload (cons %payload payload)] ,@body)]
       (assert (= ,catch-result# (cons %payload ,expected-payload))))))

(defmacro try
  "Evaluate body with optional catch and finally clauses."
  [& body]
  (let [keyfn (fn [item] (if (cons? item)
                           (case (car item)
                             %catch :catch
                             %finally :finally
                             :body)
                           :body))
        {:keys [body catch finally]} (group-by keyfn body)
        finally-forms (mapcat cdr (nreverse finally))
        body-forms (nreverse body)
        catch-forms (nreverse catch)
        ex# (gensym 'ex)
        payload# (gensym 'payload)
        outcome# (gensym 'outcome)
        value# (gensym 'value)
        ex-payload# (gensym 'ex-payload)
        ex-patterns (map (fn [[_ pattern & catch-body]]
                           `(,pattern (do ,@catch-body)))
                      catch-forms)]
    `(let [,outcome# (catch [,payload# [:error ,payload#]]
                       [:ok (catch [,ex# (case ,ex#
                                           ,@(flatten1 ex-patterns)
                                           (throw ,ex#))]
                              ,@body-forms)])]
       ,@finally-forms
       (case ,outcome#
         [:ok ,value#] ,value#
         [:error ,ex-payload#] (throw ,ex-payload#)))))

; -----------------------
; Macro Expansion Helpers
; -----------------------

(defn macro-form?*
  "Return true when form resolves to a macro in env."
  [form env]
  (if (cons? form)
    (let [head (car form)
          tail (cdr form)]
      (if (and (symbol? head) (proper-list? tail))
        (macro? (eval head env))
        false))
    false))

(defmacro macro-form?
  "Return true when form resolves to a macro in the current env."
  [form]
  `(macro-form?* ,form (curlet)))

(defn macroexpand*
  "Expand form repeatedly in env until no macro call remains."
  [form env]
  (if (macro-form?* form env)
    (macroexpand* (macroexpand-1 form env) env)
    form))

(defmacro macroexpand
  "Fully macroexpand form in the current env."
  [form]
  `(macroexpand* ,form (curlet)))

(def macroexpand-all*
  (let [old-fn fn
        map-env (old-fn [f coll env]
                  (map (old-fn [x] (f x env)) coll))
        expand-qq (old-fn
                    "Expand quasiquote contents while tracking quasiquote nesting level."
                    [form env level]
                    (case form
                      (cons x y)
                      (if (cons? y)
                        (case x
                          'quasiquote (cons 'quasiquote
                                            (expand-qq y env (+ level 1)))
                          'unquote (if (= level 1)
                                     (cons 'unquote
                                           (macroexpand-all* y env))
                                     (cons 'unquote
                                           (expand-qq y env (- level 1))))
                          'unquote-splicing (if (= level 1)
                                              (cons 'unquote-splicing
                                                    (macroexpand-all* y env))
                                              (cons 'unquote-splicing
                                                    (expand-qq y env (- level 1))))
                          (cons (expand-qq x env level)
                                (expand-qq y env level)))
                        (cons (expand-qq x env level)
                              (expand-qq y env level)))
                      (Vector x) (Vector (map-env (old-fn [item env]
                                                    (expand-qq item env level))
                                                  x
                                                  env))
                      (Map x) (Map (map-env (old-fn [item env]
                                             (expand-qq item env level))
                                            x
                                            env))
                      form))]
    (fn macroexpand-all*
      "Recursively macroexpand a form tree in env."
      [form env]
      (let [form (macroexpand* form env)]
        (case form
          (cons x y) (if (cons? y)
                       (case x
                         'quote form
                         'quasiquote (cons 'quasiquote (expand-qq y env 1))
                         (map-env macroexpand-all* form env))
                       (cons (macroexpand-all* x env) (macroexpand-all* y env)))
          (Vector x) (Vector (map-env macroexpand-all* x env))
          (Map x) (Map (map-env macroexpand-all* x env))
          form)))))

(defmacro macroexpand-all
  "Recursively macroexpand a form tree in the current env."
  [form]
  `(macroexpand-all* ,form (curlet)))

(defmacro ->
  "Thread head through forms as first argument."
  [head & rest]
  (case rest
    [[beg & end] & tail] `(-> (,beg ,head ,@end) ,@tail)
    [beg & tail] `(-> (,beg ,head) ,@tail)
    head))

(defmacro ->>
  "Thread head through forms as last argument."
  [head & rest]
  (case rest
    [[beg & end] & tail] `(->> (,beg ,@end ,head) ,@tail)
    [beg & tail] `(->> (,beg ,head) ,@tail)
    head))

(defmacro with-syms
  "Bind generated symbols for each symbol in syms while running body."
  [syms & body]
  (let [bindings (-> syms
                     (->> (map (fn [sym] [(Symbol (str sym "#")) `(gensym ',sym)])))
                     flatten1
                     Vector)]
    `(let ,bindings ,@body)))

(defn kvs->map
  "Convert an iterable of key/value pairs into a map."
  [kvs]
  (let [m {}
        it (iter kvs)]
    (while it
      (let [k @it]
        (setq it (next it))
        (if (not it)
          (raise 'kvs->map "kvs->map expects key/value pairs"))
        (let [v @it]
          (put m k v)
          (setq it (next it)))))
    m))

; --------------------------
; Class and Multimethod Core
; --------------------------

(defn class?
  "Return true if x is a class."
  [x]
  (and (map? x)
       (some? (%name x))
       (vector? (%schema x))
       (function? (%invoke x))))

(defn class
  "Return the class or type of x."
  [x]
  (if (map? x)
    (let [proto (getproto x)]
      (if (class? proto)
        proto
        (type x)))
    (type x)))

(defn multimethod?
  "Return true when x has multimethod shape."
  [x]
  (and (map? x)
       (%multimethod x)
       (function? (%dispatch x))
       (map? (%methods x))
       (function? (%invoke x))))

(defn -class-enum?
  "Return true when spec is an enum declaration."
  [spec]
  (and (vector? spec)
       (> (len spec) 0)
       (= (get spec 0) :enum)))

(defn -class-enum-contains?
  "Return true when value appears in enum spec."
  [spec value]
  (member? value (drop 1 spec)))

(defn -class-coerce-value
  "Coerce and validate a class field value against spec."
  [class-name field spec value]
  (cond
   (nil? value) nil
   (type? spec)
   (if (= (type value) spec)
     value
     (raise 'class "%s.%s expects %s, got %s"
            class-name field spec (type value)))
   (class? spec)
   (spec value)
   (-class-enum? spec)
   (if (-class-enum-contains? spec value)
     value
     (raise 'class "%s.%s expects one of %s, got %s"
            class-name field spec value))
   (raise 'class "%s.%s has unsupported schema type: %s"
          class-name field spec)))

(defn -class-compile-validators
  "Compile :validate options into a validator vector."
  [opts]
  (if (contains? opts :validate)
    (let [validate (:validate opts)]
      (cond
       (function? validate) [validate]
       (vector? validate)
       (do
         (for [validator validate]
           (if (not (function? validator))
             (raise 'class
                    "class field :validate entries should be Function, got %s"
                    (type validator))))
         validate)
       (raise 'class "class field :validate should be Function or Vector, got %s"
              (type validate))))
    []))

(defn -class-run-validators
  "Run validators for one class field value."
  [class-name field validators value]
  (for [validator validators]
    (if (not (validator value))
      (raise 'class "%s.%s failed validator" class-name field))))

(defn -class-compile-field
  "Compile one class field declaration."
  [field]
  (if (not (vector? field))
    (raise 'class "class field schema should be Vector, got %s" field))
  (let [n (len field)]
    (if (or (< n 2) (> n 3))
      (raise 'class
             "class field schema expects [field type &opt opts], got %s"
             field))
    (let [[field-name spec &opt (opts {})] field]
      (if (not (keyword? field-name))
        (raise 'class "class field name should be Keyword, got %s" field-name))
      (if (not (map? opts))
        (raise 'class "class field options should be Map, got %s" opts))
      (let [validators (-class-compile-validators opts)]
        {:name field-name
         :spec spec
         :required (if (contains? opts :required) (:required opts) false)
         :validators validators}))))

(defn -class-compile-schema
  "Compile class schema declarations."
  [schema]
  (if (not (vector? schema))
    (raise 'class "class schema should be Vector, got %s" schema))
  (mapv -class-compile-field schema))

(defn -class-split-spec
  "Split class options map from field schema entries."
  [raw-schema]
  (if (not (vector? raw-schema))
    (raise 'class "class schema should be Vector, got %s" raw-schema))
  (if (and (> (len raw-schema) 0)
           (map? (get raw-schema 0)))
    {:opts (get raw-schema 0)
     :schema (drop 1 raw-schema)}
    {:opts {}
     :schema raw-schema}))

(defn -class-resolve-parent
  "Resolve optional class parent from opts."
  [opts]
  (if (contains? opts :extends)
    (let [parent (:extends opts)]
      (if (not (class? parent))
        (raise 'class "class :extends should be Class, got %s" (type parent)))
      parent)
    nil))

(defn -class-merge-schema
  "Merge parent and child schemas while rejecting duplicate fields."
  [class-name parent-schema own-schema]
  (let [seen {}
        merged nil]
    (for [entry parent-schema]
      (let [field (:name entry)]
        (if (contains? seen field)
          (raise 'class "%s has duplicate field: %s" class-name field))
        (put seen field true)
        (setq merged (cons entry merged))))
    (for [entry own-schema]
      (let [field (:name entry)]
        (if (contains? seen field)
          (raise 'class "%s has duplicate field: %s" class-name field))
        (put seen field true)
        (setq merged (cons entry merged))))
    (Vector (nreverse merged))))

(defn -class-construct
  "Construct and validate a class instance from input map."
  [class input]
  (if (not (map? input))
    (raise 'class "%s constructor expects Map, got %s"
           (%name class) (type input)))
  (let [instance (Map input)
        schema (%schema class)]
    (for [entry schema]
      (let [field (:name entry)
            spec (:spec entry)
            required (:required entry)
            validators (:validators entry)
            present (contains? input field)]
        (if (and required (not present))
          (raise 'class "%s.%s is required" (%name class) field))
        (if present
          (let [value (-class-coerce-value (%name class) field spec (get input field))]
            (-class-run-validators (%name class) field validators value)
            (put instance field value)))))
    (setproto instance class)
    instance))

(defn -make-class
  "Create a class value from name and schema vector."
  [name schema]
  (let [{:keys [opts schema]} (-class-split-spec schema)
        parent (-class-resolve-parent opts)
        compiled-schema (-class-compile-schema schema)
        merged-schema (-class-merge-schema name
                                           (if parent (%schema parent) [])
                                           compiled-schema)
        class {%name name
               %schema merged-schema
               %own-schema compiled-schema
               %parent parent}]
    (if parent
      (setproto class parent))
    (put class %invoke (fn [self input]
                         (-class-construct self input)))
    class))

(defmacro make-class
  "Create a class value from inline field declarations."
  [name & fields]
  `(-make-class ',name [,@fields]))

(defn resolve-method
  "Resolve dispatch method or :default method for mm."
  [mm dispatch-value]
  (let [methods (%methods mm)
        method (get methods dispatch-value)]
    (if (function? method)
      method
      (let [default-method (get methods :default)]
        (if (function? default-method)
          default-method
          nil)))))

(defn add-method
  "Attach a dispatch method to a multimethod."
  [mm dispatch-value method]
  (if (not (multimethod? mm))
    (raise 'defmethod "defmethod target should be Multimethod, got %s" (type mm)))
  (if (not (function? method))
    (raise 'defmethod "defmethod value should be Function, got %s" (type method)))
  (put (%methods mm) dispatch-value method)
  mm)

(defn make-multimethod
  "Create a multimethod with dispatch-fn."
  [name dispatch-fn]
  (if (not (function? dispatch-fn))
    (raise 'defmulti "defmulti dispatch function should be Function, got %s"
           (type dispatch-fn)))
  (let [mm {%name name
            %multimethod true
            %dispatch dispatch-fn
            %methods {}}]
    (put mm %invoke
         (fn [self & args]
           (let [dispatch-value (apply (%dispatch self) args)
                 method (resolve-method self dispatch-value)]
             (if (nil? method)
               (raise 'invoke "no method in %s for dispatch value %s"
                      (%name self) dispatch-value))
             (apply method args))))
    mm))

(defmacro defmulti
  "Define a named multimethod."
  [name dispatch-fn]
  (if (symbol? name)
    `(def ,name (make-multimethod ',name ,dispatch-fn))
    (raise 'syntax "defmulti name should be Symbol, got %s" name)))

(defmacro defmethod
  "Define a class method or multimethod implementation."
  [target & rest]
  (let [method-fn (fn [doc params body]
                    (if doc
                      `(fn ,doc ,params ,@body)
                      `(fn ,params ,@body)))
        class-method-form (fn [class method]
                            (cond-match rest
                              [(String doc) params & body]
                              (if (vector? params)
                                `(put ,class ',method ,(method-fn doc params body))
                                (raise 'syntax "defmethod method params should be Vector, got %s" params))
                              [params & body]
                              (if (vector? params)
                                `(put ,class ',method ,(method-fn nil params body))
                                (raise 'syntax "defmethod method params should be Vector, got %s" params))
                              (raise 'syntax "defmethod expects params and body for Class.method target, got %s"
                                     target)))
        multimethod-form (fn [name]
                           (cond-match rest
                             [dispatch-value (String doc) params & body]
                             (if (vector? params)
                               `(add-method
                                 ,name
                                 ,dispatch-value
                                 ,(method-fn doc params body))
                               (raise 'syntax "defmethod multimethod params should be Vector, got %s"
                                      params))
                             [dispatch-value params & body]
                             (if (vector? params)
                               `(add-method
                                 ,name
                                 ,dispatch-value
                                 ,(method-fn nil params body))
                               (raise 'syntax "defmethod multimethod params should be Vector, got %s"
                                      params))
                             (raise 'syntax
                                    "defmethod expects dispatch value, params and body for multimethod target, got %s"
                                    name)))]
    (cond-match target
      ['cons class ['quote method]]
      (if (symbol? method)
        (class-method-form class method)
        (raise 'syntax "defmethod method name should be Symbol, got %s" method))
      (Symbol name)
      (multimethod-form name)
      (raise 'syntax
             "defmethod expects Class.method or multimethod Symbol target, got %s"
             target))))

(defmacro defclass
  "Define and bind a class value."
  [name & fields]
  `(def ,name (make-class ,name ,@fields)))

; ----------------
; Stream Abstractions
; ----------------

(defclass BaseStream)

(defmethod BaseStream.read-char
  "Read and return one-character String, or nil at EOF."
  [self]
  (let [part (self.read 1)]
    (if (= (len part) 0)
      nil
      part)))

(defmethod BaseStream.read-byte
  "Read one byte as Integer in 0..255, or nil at EOF."
  [self]
  (let [part (self.read-char)]
    (if (nil? part)
      nil
      (get part 0))))

(defmethod BaseStream.read-be
  "Read n bytes in big-endian order and return an Integer."
  [self (Integer n)]
  (let [class-name (%name (class self))]
    (if (<= n 0)
      (raise 'stream "%s.read-be expects positive Integer n, got %s" class-name n))
    (let [remaining n
          result 0]
      (while (> remaining 0)
        (let [byte (self.read-byte)]
          (if (nil? byte)
            (raise 'stream "unexpected EOF in %s.read-be" class-name))
          (setq result (bit-or (bit-shift-left result 8) byte)))
        (decq remaining))
      result)))

(defmethod BaseStream.read-le
  "Read n bytes in little-endian order and return an Integer."
  [self (Integer n)]
  (let [class-name (%name (class self))]
    (if (<= n 0)
      (raise 'stream "%s.read-le expects positive Integer n, got %s" class-name n))
    (let [remaining n
          shift 0
          result 0]
      (while (> remaining 0)
        (let [byte (self.read-byte)]
          (if (nil? byte)
            (raise 'stream "unexpected EOF in %s.read-le" class-name))
          (setq result (bit-or result (bit-shift-left byte shift))))
        (setq shift (+ shift 8))
        (decq remaining))
      result)))

(defmethod BaseStream.readln
  "Read a single line terminated by \\n without including the terminator."
  [self]
  (let [[line _] (self.read-until "\n")]
    line))

(defclass StringStream
  {:extends BaseStream}
  [:source String {:required true}]
  [:offset Integer {:required true}])

(defn -stream-read-end
  "Return the end index for read/peek where n<=0 means read to EOF."
  [(Integer start) (Integer source-len) (Integer n)]
  (if (<= n 0)
    source-len
    (min source-len (+ start n))))

(defn -string-match-at?
  "Return true when marker bytes match source at index at."
  [(String source) (Integer at) (String marker) (Integer marker-len)]
  (let [j 0
        ok true]
    (while (and ok (< j marker-len))
      (if (= (get source (+ at j)) (get marker j))
        (incq j)
        (setq ok false)))
    ok))

(defn -string-find-from
  "Return first marker index in source at/after start, else nil."
  [(String source) (Integer start) (String marker)]
  (let [source-len (len source)
        marker-len (len marker)]
    (if (= marker-len 0)
      (raise 'stream "read-until marker cannot be empty"))
    (let [head (get marker 0)
          i start
          limit (- source-len marker-len)
          found nil]
      (while (and (<= i limit) (nil? found))
        (if (and (= (get source i) head)
                 (-string-match-at? source i marker marker-len))
          (setq found i)
          (incq i)))
      found)))

(defmethod StringStream.eof
  "Return true when the stream offset is at or beyond source length."
  [self]
  (>= (:offset self) (len (:source self))))

(defmethod StringStream.read
  "Read n chars from the current offset; n<=0 reads to EOF."
  [self &opt ((Integer n) 0)]
  (let [source (:source self)
        start (:offset self)
        source-len (len source)
        end (-stream-read-end start source-len n)
        part (if (= start end)
               ""
               (String (slice source start end)))]
    (put self :offset end)
    part))

(defmethod StringStream.peek
  "Return n chars without consuming them; n<=0 peeks to EOF."
  [self &opt ((Integer n) 0)]
  (let [source (:source self)
        start (:offset self)
        source-len (len source)
        end (-stream-read-end start source-len n)]
    (if (= start end)
      ""
      (String (slice source start end)))))

(defmethod StringStream.read-byte
  "Read one byte as Integer in 0..255, or nil at EOF."
  [self]
  (if (self.eof)
    nil
    (let [pos (:offset self)
          byte (get (:source self) pos)]
      (put self :offset (+ pos 1))
      byte)))

(defmethod StringStream.read-until
  "Read until marker. Returns [part found?] unless keep-marker=true."
  [self (String marker) &opt (keep-marker false)]
  (let [source (:source self)
        start (:offset self)
        source-len (len source)
        marker-len (len marker)
        marker-pos (-string-find-from source start marker)]
    (if marker-pos
      (let [part-end marker-pos
            stream-end (+ marker-pos marker-len)
            part (if (= start part-end)
                   ""
                   (String (slice source start part-end)))
            part+marker (if (= start stream-end)
                          ""
                          (String (slice source start stream-end)))]
        (put self :offset stream-end)
        (if keep-marker
          part+marker
          [part true]))
      (let [part (if (= start source-len)
                   ""
                   (String (slice source start source-len)))]
        (put self :offset source-len)
        (if keep-marker
          part
          [part false])))))

(defmulti make-stream class)

(defmethod make-stream String
  "Create a StringStream from String."
  [s]
  (StringStream {:source s :offset 0}))

(defmethod make-stream StringSlice
  "Create a StringStream from StringSlice."
  [s]
  (make-stream (String s)))

(defmethod make-stream StringStream
  "Return StringStream unchanged."
  [s]
  s)

(defmethod make-stream :default
  "Raise for unsupported stream source values."
  [x]
  (raise 'stream "cannot create stream from %s" (class x)))

(def stream make-stream)

(gc)
