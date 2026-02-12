(assert (= (keyword :foo) :foo))
(assert (= (keyword "foo") :foo))
(assert (= (keyword 'foo) :foo))

(let [m {:a 5 :b 8 :c 3}]
  (assert (= (:a m) 5)))

(assert (throws? '(cast . "cannot cast Integer to Keyword") (keyword 1234)))
