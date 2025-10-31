; Type
(assert (throws? '(syntax . "Cons constructor with no arguments") (Cons)))
(assert (throws? '(syntax . "Cons constructor requires a single argument") (Cons [:a :b] [:c :d])))
(assert (= (Cons [:a :b]) '(:a . :b)))

; keyword
(let [m {:a 5 :b 8 :c 3}]
  (assert (= (:a m) 5)))

; Cons
(let [x {:value 10
         'add (fn [self & rest] (reduce + (:value self) rest))}]
  (assert (= (:value x) 10))
  (assert (= ((cons x 'add) 7 8) (+ 10 7 8)))
  (assert (= (x.add 7 8) (+ 10 7 8)))
  (assert (throws? '(invoke . "cannot find method `sub` in value of type Map") (x.sub 7 8))))

; ConsIterator
(let [c '(a b c)
      it (iter c)]
  (while it
    (assert (= (type it) ConsIterator))
    (assert (= @it (car c)))
    (setq it (it))
    (setq c (cdr c)))
  (assert (throws? '(deref . "attempt to deref consumed ConsIterator") @it))
  (assert (throws? '(invoke . "attempt to advance consumed ConsIterator") (it))))

; Vector
(let [v [1 2 3]]
  (assert (= (v 2) 3))
  (assert (throws? '(get . "attempt to index Vector with non-integer index of type Keyword") (v :foo)))
  (assert (throws? '(get . "Vector index 5 out of range (0..2)") (v 5))))

; VectorIterator
(let [v '[a b c]
      it (iter v)
      index 0]
  (while it
    (assert (= (type it) VectorIterator))
    (assert (= @it (v index)))
    (setq it (it))
    (inc index))
  (assert (throws? '(deref . "attempt to deref consumed VectorIterator") @it))
  (assert (throws? '(invoke . "attempt to advance consumed VectorIterator") (it))))

; Map
(let [m {:a 5 :b 8 :c 3}]
  (assert (= (m :b) 8)))

; MapIterator
(let [m {:a 5 :b 8 :c 3}
      it (iter m)
      items nil]
  (while it
    (assert (= (type it) MapIterator))
    (setq items (cons @it items))
    (setq it (it)))
  (assert (= (Map items) m))
  (assert (throws? '(deref . "attempt to deref consumed MapIterator") @it))
  (assert (throws? '(invoke . "attempt to advance consumed MapIterator") (it))))

; Function
(let [foo (Function {:params '[a b]
                     :body '((cons a b))
                     :evalargs false
                     :evalresult false})]
  (assert (= (foo (+ 3 2) (- 8 5)) '((+ 3 2) . (- 8 5)))))

(let [foo (Function {:params '[a b]
                     :body '((cons a b))
                     :evalargs true
                     :evalresult false})]
  (assert (= (foo (+ 3 2) (- 8 5)) '(5 . 3))))

(let [foo (Function {:params '[a b]
                     :body '(`(+ ,a ,b))
                     :evalargs false
                     :evalresult true})]
  (assert (= (foo 3 4) 7)))

;  apply
(assert (= (apply + 3 4 nil) 7))
(assert (throws? '(invoke . "rest arg should be iterable, got Integer") (apply + 3 4) 7))
(let [rest '(3 4 5)]
  (assert (= (apply + 1 2 rest) 15)))
