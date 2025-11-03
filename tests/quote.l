(assert (= '(1 2 3) '(1 2 3)))
(assert (= `(1 2 3) '(1 2 3)))

(assert (= `(throw '(syntax . "foo")) '(throw '(syntax . "foo"))))
(assert (= `(throw '(a b c . "foo")) '(throw '(a b c . "foo"))))
(assert (= `(throw '(a b c . ["foo"])) '(throw '(a b c . ["foo"]))))
(assert (= `(throw '(a b c . ("foo"))) '(throw '(a b c "foo"))))
