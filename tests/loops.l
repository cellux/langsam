(let [x 5]
  (while (< x 8)
    (inc x))
  (assert (= x 8)))
