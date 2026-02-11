(assert (= (Opword %foo) %foo))
(assert (= (Opword "foo") %foo))
(assert (= (Opword 'foo) %foo))

(assert (throws? '(cast . "cannot cast Integer to Opword") (Opword 1234)))
