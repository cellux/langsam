(assert (eq 5 5))
(assert (not (eq {} {})))

(let [x {}
      v (cons x x)]
  (assert (eq (car v) (cdr v))))

(assert (= (cmp 3 5) -1))
(assert (= (cmp 5 3) 1))
(assert (= (cmp 5 5) 0))

(assert (not= (cmp 5 5.0) 0))
(assert (not= (cmp 5.0 5) 0))
(assert (not= (cmp "foo" 'foo) 0))
(assert (not= (cmp "foo" :foo) 0))
(assert (not= (cmp "foo" %foo) 0))

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

(let [values [nil true false
              Nil Boolean String Symbol Map
              0 0.0 "foo" 'foo :foo %foo
              '(a b) '(a . b) [:a :b] {:a 1 :b 3 :c 8}
              let do unless map]]
  (for [v values] (assert (= v v)))
  (let [l (list values)]
    (while l
      (let [lhs (car l)]
        (for [rhs (cdr l)] (assert (not= lhs rhs))))
      (setq l (cdr l)))))
