(let [m {:a 1}
      ref (gep m :a)]
  (assert (= ref '(:a . 1)))
  (assert (= (reset ref 4) nil))
  (assert (= @ref 4))
  (assert (= (get m :a) 4)))

(let [m {:n 2}
      ref (gep m :n)]
  (assert (= (swap ref + 5) 7))
  (assert (= @ref 7))
  (assert (= (get m :n) 7)))

(let [x 3]
  (assert (= (swapq x * 4) 12))
  (assert (= x 12))
  (assert (= (incq x) 13))
  (assert (= (decq x) 12))
  (assert (= x 12)))
