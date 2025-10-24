(defn type? [x] (= (type x) Type))

(defn nil? [x] (= x nil))
(defn some? [x] (!= x nil))

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

(defn not [x]
  (if x false true))

(defn not= [x & rest]
  (not (= x . rest)))

(defmacro import [name]
  `(def ,name (require ,(str name))))

(defn update [x k f & rest]
  (let [oldv (get x k)
        newv (f oldv . rest)]
    (put x k newv)))

(defmacro comment [& body])

(def reset setcdr)

(defmacro setq [x val]
  `(reset (gep (curlet) ',x) ,val))

(defn swap [ref f & rest]
  (reset ref (f @ref . rest)))

(defmacro swapq [x f & rest]
  `(swap (gep (curlet) ',x) ,f ,@rest))

(defn map [f coll]
  (let [result nil
        it (iter coll)]
    (while it
      (setq result (cons (f @it) result))
      (setq it (next it)))
    (nreverse result)))

(defn reduce [f init coll]
  (let [result init
        it (iter coll)]
    (while it
      (setq result (f @it result))
      (setq it (next it)))
    result))

(def inc
  (let [add1 (fn [x] (+ x 1))]
    (macro inc [x]
      `(swapq ,x ,add1))))

(def gensym
  (let [counter 0]
    (fn gensym [&opt (prefix 'gensym)]
      (let [sym (Symbol (str prefix "#" counter))]
        (inc counter)
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

(defn even? [x] (= (mod x 2) 0))
(defn odd? [x] (not (even? x)))

(defmacro case [item & clauses]
  (let [item# (gensym 'item)
        subcase (fn [clauses]
                  (if clauses
                    (if-let [[pattern result & clauses] clauses]
                      `(if-let [,pattern ,item#]
                         ,result
                         ,(subcase clauses))
                      (if-let [[result] clauses]
                        result))))]
    `(let [,item# ,item]
       ,(subcase clauses))))

(defmacro prog1 [head & rest]
  (let [result# (gensym 'result)]
    `(let [,result# ,head]
       ,@rest
       ,result#)))

(defmacro for [bindings & body]
  (if-let [[var coll & rest] bindings]
    (let [it# (gensym 'it)
          coll# (gensym 'coll)]
      `(let [,coll# ,coll
             ,it# (iter ,coll#)
             ,var nil]
         (while ,it#
           (setq ,var @,it#)
           (for ,rest ,@body)
           (setq ,it# (next ,it#)))))
    `(do ,@body)))

(defn flatten [coll]
  (def result nil)
  (for [item coll]
    (if-let [flattened (flatten item)]
      (for [x flattened]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))

(defn iterable? [x]
  (if-let [it (iter x)]
    true false))

(defn flatten1 [coll]
  (def result nil)
  (for [item coll]
    (if (iterable? item)
      (for [x item]
        (setq result (cons x result)))
      (setq result (cons item result))))
  ((type coll) (nreverse result)))
(gc)
