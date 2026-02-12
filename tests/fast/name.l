(assert (= (name 'foo) "foo"))
(assert (= (name :foo) "foo"))
(assert (= (name %foo) "foo"))

(let [f (fn foo [x] x)]
  (assert (= (name f) "foo")))

(assert (= (name (fn [x] x)) nil))

(assert (throws? '(name . "Integer does not support name")
                 (name 1234)))
