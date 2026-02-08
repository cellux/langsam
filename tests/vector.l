(assert (= (Vector [1 2 3]) [1 2 3]))
(assert (= (Vector '(a b c)) '[a b c]))

(let [v [1 2 3]]
  (assert (= (v 2) 3))
  (assert (throws? '(get . "attempt to index Vector with Keyword") (v :foo)))
  (assert (throws? '(get . "Vector index 5 out of range (0..2)") (v 5))))

(let [v [1 2 3]]
  (assert (= (get v 1) 2))
  (assert (= (get v -1) 3))
  (assert (= (get v -3) 1))
  (assert (throws? '(get . "attempt to index Vector with Keyword")
                   (get v :zero)))
  (assert (throws? '(get . "Vector index 3 out of range (0..2)")
                   (get v 3)))
  (assert (throws? '(get . "Vector index -4 out of range (0..2)")
                   (get v -4))))

; VectorIterator
(let [v '[a b c]
      it (iter v)
      last-it nil
      index 0]
  (while it
    (assert (= (type it) VectorIterator))
    (setq last-it it)
    (assert (= @it (v index)))
    (setq it (it))
    (incq index))
  (assert (throws? '(deref . "attempt to deref consumed VectorIterator") @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed VectorIterator") (last-it))))
