(let [m {:a 1}
      c (clone m)]
  (assert (= c {:a 1}))
  (put c :b 2)
  (assert (= m {:a 1}))
  (assert (= c {:a 1 :b 2})))

(let [v [1 2 3]
      c (clone v)]
  (assert (= c [1 2 3]))
  (put c 0 9)
  (assert (= v [1 2 3]))
  (assert (= c [9 2 3])))

(assert (throws? '(clone . "Integer does not support clone")
                 (clone 1)))
(assert (throws? '(clone . "StringSlice does not support clone")
                 (clone (slice "abc" 0 2))))
(assert (throws? '(clone . "VectorSlice does not support clone")
                 (clone (slice [1 2 3] 0 2))))
