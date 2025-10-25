(let [y 8
      z 20
      x (do 5 "foo" 6 (def z 30) (+ y 1))]
  (assert (= y 8))
  (assert (= x 9))
  (assert (= z 30)))
