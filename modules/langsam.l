(defn type? [x] (= (type x) Type))

(defn nil? [x] (= x nil))
(defn some? [x] (not= x nil))

(defn boolean? [x] (= (type x) Boolean))
(defn integer? [x] (= (type x) Integer))
(defn float? [x] (= (type x) Float))
(defn string? [x] (= (type x) String))
(defn symbol? [x] (= (type x) Symbol))
(defn keyword? [x] (= (type x) Keyword))
(defn opword? [x] (= (type x) Opword))
(defn cons? [x] (= (type x) Cons))
(defn vector? [x] (= (type x) Vector))
(defn map? [x] (= (type x) Map))
(defn function? [x] (= (type x) Function))

(defn raise [kind fmt & args]
  (throw (cons kind (apply format fmt args))))

(defn not [x]
  (if x false true))

(defn not= [x & rest]
  (not (apply = x rest)))

(defn special? [x]
  (and (function? x)
       (not (:evalargs x))
       (not (:evalresult x))))

(defmacro when [cond & body]
  `(if ,cond (do ,@body)))

(defmacro unless [cond & body]
  `(if (not ,cond) (do ,@body)))

(def cond
  (let [subcond (fn [clauses]
                  (if clauses
                    (if-match [[test then & rest] clauses]
                      `(if ,test ,then ,(subcond rest))
                      (let [[fallback] clauses]
                        fallback))
                    nil))]
    (macro cond [& clauses]
      (subcond clauses))))

(defn sublet [proto &opt (size 64)]
  (let [sublet (Map size)]
    (setproto sublet proto)
    sublet))

(defmacro import [name]
  `(def ,name (require ,(str name))))

(defn update [x k f & rest]
  (let [oldv (get x k)
        newv (apply f oldv rest)]
    (put x k newv)))

(defmacro comment [& body])

(def reset setcdr)

(defmacro setq [x val]
  `(reset (gep (curlet) ',x) ,val))

(defn swap [ref f & rest]
  (reset ref (apply f @ref rest)))

(defmacro swapq [x f & rest]
  `(swap (gep (curlet) ',x) ,f ,@rest))

(defn inc [x] (+ x 1))
(defmacro incq [x] `(swapq ,x ,inc))

(defn dec [x] (- x 1))
(defmacro decq [x] `(swapq ,x ,dec))

(defn even? [x] (= (mod x 2) 0))
(defn odd? [x] (not= (mod x 2) 0))

(defn zero? [x]
  (cond-match x
    (Integer i) (= i 0)
    (Float f) (= f 0.0)
    false))

(defn nonzero? [x]
  (not (zero? x)))

(defn pos? [x]
  (cond-match x
    (Integer i) (> i 0)
    (Float f) (> f 0.0)
    false))

(defn neg? [x]
  (cond-match x
    (Integer i) (< i 0)
    (Float f) (< f 0.0)
    false))

(defn map [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (setq result (cons (f @it) result))
      (setq it (next it)))
    (nreverse result)))

(defn mapv [f coll]
  (Vector (map f coll)))

(defn reduce [f init coll]
  (let [result init
        it (iter coll)]
    (while it
      (setq result (f @it result))
      (setq it (next it)))
    result))

(defn filter [f coll]
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
  (if (nil? coll)
    nil
    (if (<= n 0)
      coll
      (let [it (iter coll)
            i 0]
        (while (and it (< i n))
          (setq it (next it))
          (incq i))
        (let [result nil]
          (while it
            (setq result (cons @it result))
            (setq it (next it)))
          (let [result (nreverse result)]
            (if (cons? coll)
              result
              ((type coll) result))))))))

(defn tail
  "Return coll without its first element."
  [coll]
  (drop 1 coll))

(def range
  (let [empty? (fn [start end step]
                 (if (pos? step)
                   (>= start end)
                   (<= start end)))
        make-state (fn [start end step]
                     (if (or (not (integer? start))
                             (not (integer? end))
                             (not (integer? step)))
                       (raise 'range "range expects Integer arguments, got %s %s %s"
                              (type start) (type end) (type step)))
                     (if (= step 0)
                       (raise 'range "range step cannot be 0"))
                     (if (empty? start end step)
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
                                               (if (empty? next-i (:end self) (:step self))
                                                 (do
                                                   (put self :consumed true)
                                                   nil)
                                                 (do
                                                   (put self :i next-i)
                                                   self))))))
                         it)))]
    (fn range
      "Return an iterator over an integer range."
      [& args]
      (let [n (len args)]
        (if (= n 1)
          (let [[end] args]
            (make-state 0 end 1))
          (if (= n 2)
            (let [[start end] args]
              (make-state start end 1))
            (if (= n 3)
              (let [[start end step] args]
                (make-state start end step))
              (raise 'range "range expects 1 to 3 args, got %s" n))))))))

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

(def gensym
  (let [counter 0]
    (fn gensym [&opt (prefix 'gensym)]
      (let [sym (Symbol (str prefix "#" counter))]
        (incq counter)
        sym))))

(defmacro and [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# (and ,@rest) ,val#)))
    x))

(defmacro or [x & rest]
  (if rest
    (let [val# (gensym 'val)]
      `(let [,val# ,x]
         (if ,val# ,val# (or ,@rest))))
    x))

(defmacro when-let [bindings & body]
  `(if-let ,bindings (do ,@body)))

(defmacro while-let [bindings & body]
  `(while (if-let ,bindings (do ,@body true) false)))

(defmacro when-match [bindings & body]
  `(if-match ,bindings (do ,@body)))

(defmacro while-match [bindings & body]
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
      (let [item# (gensym 'item)]
        `(let [,item# ,item]
           ,(subcase item# clauses))))))

(defmacro prog1 [head & rest]
  (let [result# (gensym 'result)]
    `(do (def ,result# ,head)
         ,@rest
         ,result#)))

(defmacro for [bindings & body]
  (if-match [[pat coll & rest] bindings]
    (let [it# (gensym 'it)]
      `(let [,it# (iter ,coll)]
         (while ,it#
           (let [,pat @,it#]
             (for ,rest ,@body)
             (setq ,it# (next ,it#))))))
    `(do ,@body)))

(defn group-by [keyfn coll]
  (let [groups {}
        it (iter coll)]
    (while it
      (let [item @it
            key (keyfn item)]
        (update groups key (fn [group item] (cons item group)) item))
      (setq it (next it)))
    groups))

(defn separate [pred coll]
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

(defn partition [n coll]
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

(defn comp [f g]
  (fn [& rest] (f (apply g rest))))

(defn iterable? [x]
  (catch [_ false]
    (iter x)
    true))

(defn catv [& parts]
  (let [result nil]
    (for [part parts]
      (if (iterable? part)
        (for [x part]
          (setq result (cons x result)))
        (setq result (cons part result))))
    (Vector (nreverse result))))

(defn flatten [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x (flatten item)]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defn flatten1 [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x item]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defmacro throws? [expected-payload & body]
  (let [catch-result# (gensym 'catch-result)]
    `(let [,catch-result# (catch [payload (cons %exception-payload payload)] ,@body)]
       (assert (= ,catch-result# (cons %exception-payload ,expected-payload))))))

(defmacro try [& body]
  (let [keyfn (fn [item] (if (cons? item)
                           (case (car item)
                             %catch :catch
                             %finally :finally
                             :body)
                           :body))
        {:keys [body catch finally]} (group-by keyfn body)
        finally-forms (flatten1 (map cdr finally))
        ex# (gensym 'ex)
        ex-patterns (map (fn [[_ pattern & body]]
                           `(,pattern (prog1 (do ,@body) ,@finally-forms)))
                      (nreverse catch))]
    `(catch [,ex# (case ,ex#
                    ,@(flatten1 ex-patterns)
                    (throw ,ex#))]
       ,@(nreverse body)
       ,@finally-forms)))

(defn macro-form?*
  [form env]
  (if-match [[(Symbol head) & rest] form]
    (macro? (eval head env))))

(defmacro macro-form?
  [form]
  `(macro-form?* ,form (curlet)))

(defn macroexpand*
  [form env]
  (if (macro-form?* form env)
    (macroexpand* (macroexpand-1 form env) env)
    form))

(defmacro macroexpand
  [form]
  `(macroexpand* ,form (curlet)))

(def macroexpand-all*
  (let [old-fn fn
        map-env (old-fn [f coll env]
                  (map (old-fn [x] (f x env)) coll))
        expand-qq (fn [form env]
                    (case form
                      (cons x y) (if (cons? y)
                                   (case x
                                     'unquote (cons 'unquote
                                                    (macroexpand-all* y env))
                                     'unquote-splicing (cons 'unquote-splicing
                                                             (macroexpand-all* y env))
                                     form)
                                   (cons (expand-qq x env)
                                         (expand-qq y env)))
                      (Vector x) (Vector (map-env expand-qq x env))
                      (Map x) (Map (map-env expand-qq x env))
                      form))]
    (fn macroexpand-all*
      [form env]
      (let [form (macroexpand* form env)]
        (case form
          (cons x y) (if (cons? y)
                       (case x
                         'quote form
                         'quasiquote (cons 'quasiquote (expand-qq y env))
                         (map-env macroexpand-all* form env))
                       (cons (macroexpand-all* x env) (macroexpand-all* y env)))
          (Vector x) (Vector (map-env macroexpand-all* x env))
          (Map x) (Map (map-env macroexpand-all* x env))
          form)))))

(defmacro macroexpand-all
  [form]
  `(macroexpand-all* ,form (curlet)))

(defmacro -> [head & rest]
  (case rest
    [[beg & end] & tail] `(-> (,beg ,head ,@end) ,@tail)
    [beg & tail] `(-> (,beg ,head) ,@tail)
    head))

(defmacro ->> [head & rest]
  (case rest
    [[beg & end] & tail] `(->> (,beg ,@end ,head) ,@tail)
    [beg & tail] `(->> (,beg ,head) ,@tail)
    head))

(defmacro with-syms [syms & body]
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

(defn class?
  [x]
  (and (map? x)
       (some? (%name x))
       (vector? (%schema x))
       (function? (%invoke x))))

(defn multimethod?
  [x]
  ((get (require "multimethod") 'multimethod?) x))

(defmacro make-class
  [name & fields]
  `((get (require "class") 'make-class) ',name [,@fields]))

(defmacro defmulti
  [name dispatch-fn]
  (if (symbol? name)
    `(def ,name ((get (require "multimethod") 'multimethod)
                 ',name
                 ,dispatch-fn))
    (raise 'syntax "defmulti name should be Symbol, got %s" name)))

(defmacro defmethod
  [target & rest]
  (if-match [['cons class ['quote method]] target]
    (if-match [[params & body] rest]
      (if (symbol? method)
        `(put ,class ',method (fn ,params ,@body))
        (raise 'syntax "defmethod method name should be Symbol, got %s" method))
      (raise 'syntax "defmethod expects params and body for Class.method target, got %s"
             target))
    (if (symbol? target)
      (if-match [[dispatch-value params & body] rest]
        (if (vector? params)
          `((get (require "multimethod") 'add-method)
            ,target
            ,dispatch-value
            (fn ,params ,@body))
          (raise 'syntax "defmethod multimethod params should be Vector, got %s"
                 params))
        (raise 'syntax
               "defmethod expects dispatch value, params and body for multimethod target, got %s"
               target))
      (raise 'syntax
             "defmethod expects Class.method or multimethod Symbol target, got %s"
             target))))

(defmacro defclass
  [name & fields]
  `(def ,name (make-class ,name ,@fields)))

(gc)
