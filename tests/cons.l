(let [c (cons :a :b)]
  (assert (= (type c) Cons))
  (assert (= (car c) :a))
  (assert (= (cdr c) :b)))

(let [c (cons :a :b :c)]
  (assert (= (type c) Cons))
  (assert (= (car c) :a))
  (assert (= (cdr c) '(:b . :c))))

(let [c '(a b c . d)]
  (assert (= (type c) Cons))
  (assert (= (car c) 'a))
  (assert (= (cdr c) '(b c . d))))

(let [c (cons :a :b)]
  (assert (= (repr c) "(:a . :b)")))

(assert (= (cons :a :b) (Cons (cons :a :b))))
(assert (= (cons :a :b) (Cons '(:a . :b))))
(assert (= (cons :a :b) '(:a . :b)))

(assert (= (type '(:a :b)) Cons))
(assert (= (type '(1 2 3)) Cons))
(assert (= (Cons [1 2 3]) '(1 2 . 3)))
(assert (= (Cons (cons 1 (cons 2 (cons 3 nil)))) '(1 2 3)))
(assert (= (type (Cons '(1 2 . 3))) Cons))

(assert (= (list :a :b) (cons :a :b nil)))
