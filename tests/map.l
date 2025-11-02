(let [m {:foo 5 :bar 8}]
  (assert (= (m :foo) 5))
  (assert (= (m :bar) 8))
  (put m :bar 3)
  (assert (= (m :foo) 5))
  (assert (= (m :bar) 3))
  (update m :bar * 5)
  (assert (= (m :bar) 15)))

(let [obj {:value 3
           'add (fn [self amount]
                  (update self :value + amount))}]
  (assert (= (:value obj) 3))
  (obj.add 5)
  (assert (= (:value obj) 8)))

(let [m {:a 5 :b 3}]
  (setproto m {:c 8 :d 4})
  (assert (= (m :a) 5))
  (assert (= (m :b) 3))
  (assert (= (m :c) 8))
  (assert (= (m :d) 4))
  (assert (= (gep m :a) '(:a . 5)))
  (assert (= (gep m :d) '(:d . 4)))
  (let [item (gep m :c)]
    (assert (cons? item))
    (assert (= (car item) :c))
    (assert (= (cdr item) 8))
    (assert (= @item 8))))

(let [m {:a 5 :b 3}]
  (assert (= (len m) 2))
  (put m :c nil)
  (assert (= (len m) 3))
  (del m :c)
  (assert (= (len m) 2)))

(let [m1 {:a 5 :b 3 :c nil}
      m2 {:a 5 :b 3}]
  (assert (not= m1 m2))
  (assert (not= m2 m1)))

(assert (= (Map {:a 3 :b 5})) {:a 3 :b 5})
(assert (= (Map (map (fn [x] x) {:a 3 :b 5})) {:a 3 :b 5}))
