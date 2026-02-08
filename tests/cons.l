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

(assert (= (Cons '(a . b)) '(a . b)))
(assert (= (Cons '(a b c)) '(a b c)))
(assert (= (Cons '[a b c]) '(a b . c)))

(assert (= (type '(:a :b)) Cons))
(assert (= (type '(1 2 3)) Cons))
(assert (= (Cons [1 2 3]) '(1 2 . 3)))
(assert (= (Cons (cons 1 (cons 2 (cons 3 nil)))) '(1 2 3)))
(assert (= (type (Cons '(1 2 . 3))) Cons))

(assert (= (list :a :b) (cons :a :b nil)))

(assert (throws? '(syntax . "Cons constructor with no argument") (Cons)))
(assert (throws? '(syntax . "Cons constructor requires a single argument") (Cons [:a :b] [:c :d])))
(assert (= (Cons [:a :b]) '(:a . :b)))

(let [c (cons :a :b)]
  (assert (= (get c 0) :a))
  (assert (= (get c 1) :b))
  (assert (throws? '(get . "attempt to index Cons with Keyword")
                   (get c :car)))
  (assert (throws? '(get . "Cons index -2 out of range (0..1)")
                   (get c -2))))

(let [x {:value 10
         'add (fn [self & rest] (reduce + (:value self) rest))}]
  (assert (= (:value x) 10))
  (assert (= ((cons x 'add) 7 8) (+ 10 7 8)))
  (assert (= (x.add 7 8) (+ 10 7 8)))
  (assert (throws? '(invoke . "cannot find method `sub` in Map") (x.sub 7 8))))

; ConsIterator
(let [c '(a b c)
      it (iter c)
      last-it nil]
  (while it
    (assert (= (type it) ConsIterator))
    (setq last-it it)
    (assert (= @it (car c)))
    (setq it (it))
    (setq c (cdr c)))
  (assert (throws? '(deref . "attempt to deref consumed ConsIterator") @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed ConsIterator") (last-it))))
