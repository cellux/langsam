(let [x {:value 10
         'add (fn [self & rest] (reduce + (:value self) rest))}]
  (assert (= (:value x) 10))
  (let [dotted-args '(9 10)]
    (assert (= ((cons x 'add) 7 8 . dotted-args) (+ 10 7 8 9 10)))
    (assert (= (x.add 7 8 . dotted-args) (+ 10 7 8 9 10)))))
