(let [m {:foo 5 :bar 8}]
  (assert (= (m :foo) 5))
  (assert (= (m :bar) 8)))
