(let [x 5]
  (assert (= x 5))
  (do
    (def x 6)
    (def [y z] [x (* 2 x)])
    (assert (= x 6))
    (assert (= y 6))
    (assert (= z 12)))
  (assert (= x 6)))
