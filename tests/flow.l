(let [y 8
      z 20
      x (do 5 "foo" 6 (def z 30) (prog1 (+ y 1) (put (curlet) 'y 11)))]
  (assert (= y 11))
  (assert (= x 9))
  (assert (= z 30)))

(let [x 5]
  (while (< x 8)
    (incq x))
  (assert (= x 8)))

(let [coll [1 2 3 4]
      it (iter coll)
      result nil]
  (while it
    (setq result (cons @it result))
    (setq it (next it)))
  (assert (= result '(4 3 2 1))))
