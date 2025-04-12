(def x 5)
(assert (= x 5))
(let [x 6] (assert (= x 6)))
(let [x 6 y 3]
  (assert (= x 6))
  (assert (= y 3)))
(assert (= x 5))
