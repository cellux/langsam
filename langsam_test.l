;; Nil

(assert (= (type nil) Nil))
(assert (= (Nil 'foo) nil))
(assert (= (str nil) "nil"))
(assert (= (repr nil) "nil"))

;; Error

(let [e (error "foo")]
  (assert (= (type e) Error))
  (assert (= (type (Error e)) Error))
  (assert (= @e "foo")))

(let [e (error "this failed %d times" 5)]
  (assert (= @e "this failed 5 times"))
  (assert (= (str e) "this failed 5 times"))
  (assert (= (repr e) "(error '\"this failed 5 times\")")))

(let [e (error [:special :error])]
  (assert (= (type e) Error))
  (assert (= @e [:special :error]))
  (assert (= (str e) "[:special :error]"))
  (assert (= (repr e) "(error '[:special :error])")))

(let [e (error '(:not-found . "this is a 404 error"))]
  (assert (= @e '(:not-found . "this is a 404 error")))
  (assert (= (str e) "this is a 404 error"))
  (assert (= (repr e) "(error '(:not-found . \"this is a 404 error\"))")))

;; Boolean

(assert (= true true))
(assert (= false false))

(assert (!= true false))
(assert (!= false true))

(assert (= (type true) Boolean))
(assert (= (type false) Boolean))

(assert (= (eval true) true))
(assert (= (eval false) false))

(assert (= (Boolean true) true))
(assert (= (Boolean false) false))
(assert (= (Boolean nil) false))
(assert (= (Boolean '()) false))
(assert (= (Boolean '(:a)) true))
(assert (= (Boolean []) false))
(assert (= (Boolean [:a]) true))
(assert (= (Boolean "") false))
(assert (= (Boolean "0") true))
(assert (= (Boolean 0) false))
(assert (= (Boolean -1) true))
(assert (= (Boolean 0.0) false))
(assert (= (Boolean 0.5) true))
(assert (= (Boolean -1.0) true))
(assert (= (Boolean 'foo) true))
(assert (= (Boolean :foo) true))
(assert (= (Boolean +) true))
(assert (= (Boolean '(:a . :b)) true))

(assert (= (str true) "true"))
(assert (= (str false) "false"))

(assert (= (repr true) "true"))
(assert (= (repr false) "false"))

;; Integer

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
(assert (= (str "foo") "foo"))
(assert (= (str "foo" "bar") "foobar"))
(assert (= (str "foo" 523 "bar") "foo523bar"))

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

(let [c '(a b c . d)]
  (assert (= (type c) Cons))
  (assert (= (car c) 'a))
  (assert (= (cdr c) '(b c . d))))

(let [c (cons :a :b)]
  (assert (= (repr c) "(:a . :b)")))

(assert (= (cons :a :b) (Cons (cons :a :b))))
(assert (= (cons :a :b) '(:a . :b)))

(assert (= (type '(:a :b)) List))
(assert (= (type '(1 2 3)) List))
(assert (= (List [1 2 3]) '(1 2 3)))
(assert (= (List (cons 1 (cons 2 (cons 3 nil)))) '(1 2 3)))
(assert (= (List `(1 2 3 . ,nil)) '(1 2 3)))
(assert (= (type (List '(1 2 . 3))) Error))

(let [f (Function {:params '[x] :body '[(+ x 5)]})]
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
