(assert (= true true))
(assert (= false false))

(assert (= (not true) false))
(assert (= (not false) true))

(assert (not= true false))
(assert (not= false true))

(assert (= (type true) Boolean))
(assert (= (type false) Boolean))

(assert (= (eval true) true))
(assert (= (eval false) false))

(assert (= (Boolean true) true))
(assert (= (Boolean false) false))
(assert (= (Boolean nil) false))
(assert (= (Boolean '()) false))
(assert (= (Boolean '(:a)) true))
(assert (= (Boolean []) true))
(assert (= (Boolean [:a]) true))
(assert (= (Boolean "") true))
(assert (= (Boolean "0") true))
(assert (= (Boolean 0) true))
(assert (= (Boolean 1) true))
(assert (= (Boolean -1) true))
(assert (= (Boolean 0.0) true))
(assert (= (Boolean 0.5) true))
(assert (= (Boolean -1.0) true))
(assert (= (Boolean 'foo) true))
(assert (= (Boolean :foo) true))
(assert (= (Boolean +) true))
(assert (= (Boolean '(:a . :b)) true))

(for [x [true false]] (assert (= (Boolean x) x)))

(assert (= (str true) "true"))
(assert (= (str false) "false"))

(assert (= (repr true) "true"))
(assert (= (repr false) "false"))

(assert (= (and 5 3) 3))
(assert (= (and 0 3) 3))
(assert (= (and false 3) false))
(assert (= (and nil 3) nil))
(assert (= (and [] 3) 3))

(assert (= (or 5 3) 5))
(assert (= (or 0 3) 0))
(assert (= (or false 3) 3))
(assert (= (or nil 3) 3))
(assert (= (or {} 3) {}))
