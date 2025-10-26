(let [y 8
      z 20
      x (do 5 "foo" 6 (def z 30) (prog1 (+ y 1) (put (curlet) 'y 11)))]
  (assert (= y 11))
  (assert (= x 9))
  (assert (= z 30)))
