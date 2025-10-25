(assert (eq 5 5))
(assert (not (eq {} {})))

(let [x {}
      v (cons x x)]
  (assert (eq (car v) (cdr v))))

(assert (= (cmp 3 5) -1))
(assert (= (cmp 5 3) 1))
(assert (= (cmp 5 5) 0))

(assert (< 3 5))
(assert (<= 3 5))
(assert (<= 3 3))
(assert (not (<= 5 3)))
(assert (not (< 5 5)))

(assert (not (> 3 5)))
(assert (not (>= 3 5)))
(assert (>= 3 3))
(assert (>= 5 3))
(assert (not (> 5 5)))

(assert (= 1 1))
(assert (= 1.0 1.0))
(assert (not= 1 1.0))
(assert (not= 1.0 1))
