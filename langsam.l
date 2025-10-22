(defn type? [x] (= (type x) Type))

(defn nil? [x] (= (type x) Nil))
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
      (let [sym (Symbol (str prefix "-" counter))]
        (inc counter)
        sym))))

(defmacro and [x & rest]
  (if rest
    (let [val (gensym 'val)]
      `(let [,val ,x]
         (if ,val (and ,@rest) ,val)))
    x))

(defmacro or [x & rest]
  (if rest
    (let [val (gensym 'val)]
      `(let [,val ,x]
         (if ,val ,val (or ,@rest))))
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

(gc)
