(assert (= (nreverse '(1 2 3))
           '(3 2 1)))

(assert (= (group-by car '((foo bar) (bar goo) (foo baz)))
           {'foo '((foo baz) (foo bar)) 'bar '((bar goo))}))

(assert (= (separate (fn [x] (> x 3)) [1 2 3 4 5 6 7])
           ['(4 5 6 7) '(1 2 3)]))

(assert (= (separate even? [1 2 3 4 5 6 7])
           ['(2 4 6) '(1 3 5 7)]))
