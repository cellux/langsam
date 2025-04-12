(let [c (cons :a :b)]
  (assert (= (type c) Cons))
  (assert (= (car c) :a))
  (assert (= (cdr c) :b)))

(let [c '(a b c . d)]
  (assert (= (type c) Cons))
  (assert (= (car c) 'a))
  (assert (= (cdr c) '(b c . d))))

(let [c (cons :a :b)]
  (assert (= (repr c) "(:a . :b)")))

(assert (= (cons :a :b) (Cons (cons :a :b))))
(assert (= (cons :a :b) '(:a . :b)))
