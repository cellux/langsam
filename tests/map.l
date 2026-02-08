(assert (= (Map 8) {}))

(assert (= (kvs->map []) {}))
(assert (= (kvs->map [:a 3 :b 5 :c 8]) {:a 3 :b 5 :c 8}))
(assert (= (kvs->map '(:a 3 :b 5 :c 8)) {:a 3 :b 5 :c 8}))
(assert (= (kvs->map [:a 3 :a 5]) {:a 5}))
(assert (throws? '(kvs->map . "kvs->map expects key/value pairs")
                 (kvs->map [:a 3 :b])))

(let [m {:a 3 :b 5 :c 8}]
  (assert (= (Map m) m))
  (assert (= (Map [[:a 3] [:b 5] [:c 8]]) m))
  (assert (= (Map '((:a . 3) (:b . 5) (:c . 8))) m))
  (assert (= (Map '((:a 3) (:b 5) (:c 8))) {:a '(3) :b '(5) :c '(8)})))

(let [m {:a 3 :b 5 :c 8}]
  (assert (= (get m :b) 5))
  (assert (= (m :b) 5))
  (assert (= (:b m) 5))
  (assert (= (:d m) nil))
  (assert (= (get m 'do) nil))
  (setproto m (curlet))
  (assert (= (get m 'do) do))
  (assert (= m/do do))
  (assert (= (type m/do) Function)))

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

(let [m {:a nil}
      p {:b 2}]
  (setproto m p)
  (assert (contains? m :a))
  (assert (contains? m :b))
  (assert (not (contains? m :c)))
  (assert (= (get m :a) nil)))

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
(assert (= (len (map (fn [x] (+ x 1)) [1 2 3])) 3))
(assert (= (len (map (fn [x] (+ x 1)) [])) 0))

(let [m {:a 5 :b 8 :c 3}]
  (assert (= (m :b) 8)))

; MapIterator
(let [m {:a 5 :b 8 :c 3}
      it (iter m)
      last-it nil
      items nil]
  (while it
    (assert (= (type it) MapIterator))
    (setq last-it it)
    (setq items (cons @it items))
    (setq it (it)))
  (assert (= (Map items) m))
  (assert (throws? '(deref . "attempt to deref consumed MapIterator") @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed MapIterator") (last-it))))
