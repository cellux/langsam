(assert (= '(1 2 3) '(1 2 3)))
(assert (= `(1 2 3) '(1 2 3)))

(assert (= `(throw '(syntax . "foo")) '(throw '(syntax . "foo"))))
(assert (= `(throw '(a b c . "foo")) '(throw '(a b c . "foo"))))
(assert (= `(throw '(a b c . ["foo"])) '(throw '(a b c . ["foo"]))))
(assert (= `(throw '(a b c . ("foo"))) '(throw '(a b c "foo"))))

(assert (= `foo 'foo))
(assert (= `:foo :foo))
(assert (= `(a b . "c") '(a b . "c")))

(let [x 2]
  (assert (= `(1 ,x 3) '(1 2 3))))

(assert (= `(1 ,(+ 1 2) 4) '(1 3 4)))
(assert (= `(1 ,@'(2 3) 4) '(1 2 3 4)))
(assert (= `[1 ,@'(2 3) 4] [1 2 3 4]))

(assert (= `{:a ,(+ 1 2) :b 4} {:a 3 :b 4}))
