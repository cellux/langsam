(assert (= (type :foo) Keyword))
(assert (= (Keyword :foo) :foo))
(assert (= (Keyword 'foo) :foo))
(assert (= (Keyword "foo") :foo))
