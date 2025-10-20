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
  (not (apply = x rest)))

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
