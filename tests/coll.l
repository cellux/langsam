(assert (= (nreverse '(1 2 3)) '(3 2 1)))

(assert (= (separate (fn [x] (> x 3)) [1 2 3 4 5 6 7])
           ['(4 5 6 7) '(1 2 3)]))
