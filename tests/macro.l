(let [m (macro [x] `(+ ,x 1))
      y 5]
  (assert (macro? m))
  (assert (macro-form? '(m 3)))
  (assert (= (m (* y 3)) 16))
  (assert (= (macroexpand '(m 3)) '(+ 3 1))))
