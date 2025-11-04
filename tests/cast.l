(let [thrown-payload :foo
      caught-payload (catch [x x] (Exception thrown-payload))]
  (assert (= caught-payload thrown-payload)))

(for [x [true false]] (assert (= (Boolean x) x)))
(assert (= (Boolean 1) true))
(assert (= (Boolean 0) false))

(for [x [-5 0 5]] (assert (= (Integer x) x)))
(assert (= (Integer nil) 0))
(assert (= (Integer true) 1))
(assert (= (Integer false) 0))
(assert (= (Integer 1.4) 1))
(assert (= (Integer 1.5) 2))
(assert (= (Integer "1234") 1234))
(assert (= (Integer "-1234") -1234))
(assert (throws? '(cast . "String->Integer conversion failed for: foo") (Integer "foo")))
(assert (throws? '(cast . "Cannot cast Symbol to Integer") (Integer 'foo)))

(for [x [-5.0 0.0 5.0]] (assert (= (Float x) x)))
(assert (= (Float nil) 0.0))
(assert (= (Float true) 1.0))
(assert (= (Float false) 0.0))
(assert (= (Float 3) 3.0))
(assert (= (Float -3) -3.0))
(assert (= (Float "1234") 1234.0))
(assert (= (Float "1234.375") 1234.375))
(assert (= (Float "-1234.375") -1234.375))
(assert (throws? '(cast . "String->Float conversion failed for: foo") (Float "foo")))
(assert (throws? '(cast . "Cannot cast Symbol to Float") (Float 'foo)))

(assert (= (String "foo") "foo"))
(assert (= (String String) "String"))
(assert (= (String nil) "nil"))
(assert (= (String true) "true"))
(assert (= (String false) "false"))
(assert (= (String 1234) "1234"))
(assert (= (String 1234.375) "1234.375"))
(assert (= (String -1234.375) "-1234.375"))
(assert (= (String 0x100) "256"))
(assert (= (String 'foo) "foo"))
(assert (= (String :foo) ":foo"))
(assert (= (String %foo) "%foo"))
(assert (= (String (cons :a :b)) "(:a . :b)"))
(assert (= (String (cons :a (cons :b nil))) "(:a :b)"))
(assert (= (String [1 2 3]) "[1 2 3]"))
(let [m {:a 3 :b 5 :c 8}]
  (assert (= (read-string (String m)) m)))
(assert (= (String do) "<Special:do>"))
(assert (= (String unless) "<Macro:unless>"))
(assert (= (String map) "<Function:map>"))

(assert (= (Symbol 'foo) 'foo))
(assert (= (Symbol "foo") 'foo))
(assert (throws? '(cast . "Cannot cast Integer to Symbol") (Symbol 1234)))

(assert (= (Keyword :foo) :foo))
(assert (= (Keyword "foo") :foo))
(assert (= (Keyword 'foo) :foo))
(assert (throws? '(cast . "Cannot cast Integer to Keyword") (Keyword 1234)))

(assert (= (Opword %foo) %foo))
(assert (= (Opword "foo") %foo))
(assert (= (Opword 'foo) %foo))
(assert (throws? '(cast . "Cannot cast Integer to Opword") (Opword 1234)))

(assert (= (Cons '(a . b)) '(a . b)))
(assert (= (Cons '(a b c)) '(a b c)))
(assert (= (Cons '[a b c]) '(a b . c)))

(assert (= (Vector [1 2 3]) [1 2 3]))
(assert (= (Vector '(a b c)) '[a b c]))

(assert (= (Map 8) {}))
(let [m {:a 3 :b 5 :c 8}]
  (assert (= (Map m) m))
  (assert (= (Map [[:a 3] [:b 5] [:c 8]]) m))
  (assert (= (Map '((:a . 3) (:b . 5) (:c . 8))) m))
  (assert (= (Map '((:a 3) (:b 5) (:c 8))) {:a '(3) :b '(5) :c '(8)})))

(assert (= (Function map) map))
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
