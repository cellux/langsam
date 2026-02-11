(assert (= (Symbol 'foo) 'foo))
(assert (= (Symbol "foo") 'foo))

(assert (throws? '(cast . "cannot cast Integer to Symbol") (Symbol 1234)))
