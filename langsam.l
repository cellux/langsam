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
