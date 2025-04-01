(assert (= (+ 2 3) 5))
(assert (= (- 7 4) 3))
(assert (= (* 7 4) 28))
(assert (= (/ 28 4) 7))
(assert (= (/ 29 4) 7))
(assert (= (/ -29 4) -7))

(assert (= 0.0 0.0))

(assert (= (* 5 0.0) 0))
(assert (= (* 5.0 0.0) 0.0))
(assert (= (* 5.0 0) 0.0))

(assert (= ((fn [x] x) 5) 5))
(assert (= ((fn [x y] (+ x y)) 2 3) 5))

(def x 5)
(assert (= x 5))
