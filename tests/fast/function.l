(assert (= (Function map) map))

(assert (= (identity 7) 7))
(assert (= (identity nil) nil))

(let [foo (Function {:name 'foo
                     :params '[a b]
                     :doc "docstring"
                     :body '((+ a b))
                     :evalargs true
                     :evalresult false})]
  (assert (= (:name foo) 'foo))
  (assert (= (:params foo) '[a b]))
  (assert (= (:doc foo) "docstring"))
  (assert (= (:body foo) '((+ a b))))
  (assert (= (:evalargs foo) true))
  (assert (= (:evalresult foo) false))
  (assert (= (foo 5 3) 8)))

(let [f (fn foo [x & rest] (apply str x rest))]
  (assert (= (get f :name) 'foo))
  (assert (= (get f :params) '[x & rest]))
  (assert (= (get f :body) '((apply str x rest))))
  (assert (= (get f :evalargs) true))
  (assert (= (get f :evalresult) false)))

(let [f (Function {:params '[x] :body '((+ x 5))})]
  (assert (= (type f) Function))
  (assert (= (f 3) 8)))

(assert (= ((fn [x] x) 5) 5))
(assert (= ((fn [x y] (+ x y)) 2 3) 5))

(let [f (fn [&opt ((Integer n) 0)] n)]
  (assert (= (f) 0))
  (assert (= (f 7) 7))
  (assert (throws? '(bind . "type mismatch: (Integer n)")
                   (f 7.5))))

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
