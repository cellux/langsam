(for [x [-5.0 0.0 5.0]] (assert (= (Float x) x)))

(assert (= (Float nil) 0.0))
(assert (= (Float true) 1.0))
(assert (= (Float false) 0.0))
(assert (= (Float 3) 3.0))
(assert (= (Float -3) -3.0))
(assert (= (Float "1234") 1234.0))
(assert (= (Float "1234.375") 1234.375))
(assert (= (Float "-1234.375") -1234.375))

(assert (throws? '(cast . "conversion from String to Float failed for: foo") (Float "foo")))
(assert (throws? '(cast . "cannot cast Symbol to Float") (Float 'foo)))
