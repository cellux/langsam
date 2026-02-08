(assert (= (cond) nil))

(assert (= (cond false :a
                 true :b
                 :fallback)
           :b))

(assert (= (cond false :a
                 false :b
                 :fallback)
           :fallback))

(assert (= (cond false :a
                 nil :b)
           nil))

(let [n 0]
  (assert (= (cond (do (incq n) false) :a
                   (do (incq n) true) :b
                   (do (incq n) true) :c)
             :b))
  (assert (= n 2)))
