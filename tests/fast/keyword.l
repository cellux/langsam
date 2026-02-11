(assert (= (Keyword :foo) :foo))
(assert (= (Keyword "foo") :foo))
(assert (= (Keyword 'foo) :foo))

(let [m {:a 5 :b 8 :c 3}]
  (assert (= (:a m) 5)))

(assert (throws? '(cast . "cannot cast Integer to Keyword") (Keyword 1234)))
