(let [x 5]
  (while (< x 8)
    (inc x))
  (assert (= x 8)))

(let [coll [1 2 3 4]
      it (iter coll)
      result nil]
  (while it
    (setq result (cons @it result))
    (setq it (next it)))
  (assert (= result '(4 3 2 1))))
