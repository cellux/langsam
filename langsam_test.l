(assert (= (type nil) Nil))
(assert (= (Nil 'foo) nil))

(assert (= (type true) Boolean))
(assert (= (type false) Boolean))
(assert (= (Boolean true) true))
(assert (= (Boolean false) false))

(assert (= (type 0) Integer))
(assert (= (type 1) Integer))
(assert (= (Integer 5) 5))
(assert (= (Integer 5.3) 5))
(assert (= (Integer 5.5) 6))
(assert (= (Integer 5.7) 6))

(assert (= (type 0.0) Float))
(assert (= (type 1.0) Float))
(assert (= (Float 5) 5.0))
(assert (= (Float 5.3) 5.3))

(assert (= (type "foo") String))
(assert (= (String "foo") "foo"))
(assert (= (String "foo" "bar") "foobar"))
(assert (= (String "foo" 523 "bar") "foo523bar"))

(assert (= (type 'foo) Symbol))
(assert (= (Symbol 'foo) 'foo))
(assert (= (Symbol "foo") 'foo))

(assert (= (type :foo) Keyword))
(assert (= (Keyword :foo) :foo))
(assert (= (Keyword 'foo) :foo))
(assert (= (Keyword "foo") :foo))

(let [c (cons :a :b)]
  (assert (= (type c) Cons))
  (assert (= (car c) :a))
  (assert (= (cdr c) :b)))

(let [f (Function {:param-names '[x] :body '[(+ x 5)]})]
  (assert (= (type f) Function))
  (assert (= (f 3) 8)))

(assert (= (eval (+ 5 3)) 8))

(assert (= (+ 2 3) 5))
(assert (= (- 7 4) 3))
(assert (= (* 7 4) 28))
(assert (= (/ 28 4) 7))
(assert (= (/ 29 4) 7))
(assert (= (/ -29 4) -7))

(assert (= 0.0 0.0))

(assert (= (* 5 0.0) 0))
(assert (= (* 5.0 0.0) 0.0))
(assert (= (* 5.0 0) 0.0))

(assert (= ((fn [x] x) 5) 5))
(assert (= ((fn [x y] (+ x y)) 2 3) 5))

(def x 5)
(assert (= x 5))
(let [x 6] (assert (= x 6)))
(let [x 6 y 3]
  (assert (= x 6))
  (assert (= y 3)))
(assert (= x 5))

(assert (= '(1 2 3) '(1 2 3)))
(assert (= `(1 2 3) '(1 2 3)))

(let [m (macro [x] `(+ ,x 1))
       y 5]
  (assert (= (m (* y 3)) 16)))
