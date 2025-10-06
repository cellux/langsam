(let [y 8
      x (do 5 "foo" 6 (+ y 1))]
  (assert (= x 9)))
