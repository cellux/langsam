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
