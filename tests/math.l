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

(assert (= (mod 25 7) 4))
(assert (= (mod 25.125 7) 4.125))
