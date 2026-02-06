(for [x [-5 0 5]] (assert (= (Integer x) x)))

(assert (= (Integer nil) 0))
(assert (= (Integer true) 1))
(assert (= (Integer false) 0))
(assert (= (Integer 1.4) 1))
(assert (= (Integer 1.5) 2))
(assert (= (Integer "1234") 1234))
(assert (= (Integer "-1234") -1234))

(assert (throws? '(cast . "conversion from String to Integer failed for: foo") (Integer "foo")))
(assert (throws? '(cast . "cannot cast Symbol to Integer") (Integer 'foo)))
