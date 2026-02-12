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

(assert (= (quasiquote (a (quasiquote (b (unquote (+ 1 2))))))
           '(a (quasiquote (b (unquote (+ 1 2)))))))

(assert (= (quasiquote (a (quasiquote (b (unquote-splicing [1 2])))))
           '(a (quasiquote (b (unquote-splicing [1 2]))))))

(let [x 7]
  (assert (= (quasiquote (quasiquote (unquote (unquote x))))
             '(quasiquote (unquote 7)))))

; These use explicit forms because reader sugar cannot represent
; malformed unquote/unquote-splicing calls without changing the error path.
(assert (throws? '(syntax . "unquote needs argument")
                 (quasiquote (unquote))))

(assert (throws? '(syntax . "unquote-splicing needs argument")
                 (quasiquote (unquote-splicing))))

(assert (throws? '(syntax . "unquote expects a single argument")
                 (quasiquote (unquote 1 2))))

(assert (throws? '(syntax . "unquote-splicing expects a single argument")
                 (quasiquote (unquote-splicing [1 2] [3 4]))))
