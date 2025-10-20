(let [m {:foo 5 :bar 8}]
  (assert (= (m :foo) 5))
  (assert (= (m :bar) 8))
  (put m :bar 3)
  (assert (= (m :foo) 5))
  (assert (= (m :bar) 3))
  (update m :bar * 5)
  (assert (= (m :bar) 15)))

(let [obj {:value 3
           %add (fn [self amount]
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
