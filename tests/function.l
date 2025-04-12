(let [f (Function {:params '[x] :body '[(+ x 5)]})]
  (assert (= (type f) Function))
  (assert (= (f 3) 8)))

(assert (= ((fn [x] x) 5) 5))
(assert (= ((fn [x y] (+ x y)) 2 3) 5))
