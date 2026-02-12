(assert (= (symbol 'foo) 'foo))
(assert (= (symbol "foo") 'foo))

(assert (throws? '(cast . "cannot cast Integer to Symbol") (symbol 1234)))
