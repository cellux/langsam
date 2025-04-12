(let [m (macro [x] `(+ ,x 1))
       y 5]
  (assert (= (m (* y 3)) 16)))
