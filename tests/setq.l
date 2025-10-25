(let [x 5 y 6]
  (assert (= x 5))
  (assert (= y 6))
  (let [x 8]
    (assert (= x 8))
    (assert (= y 6))
    (setq x 7)
    (setq y 3)
    (assert (= x 7))
    (assert (= y 3))
    (let [addn (fn [x amount] (+ x amount))]
      (swapq x addn 9))
    (assert (= x 16))
    (inc x)
    (assert (= x 17)))
  (assert (= x 5))
  (assert (= y 3)))
