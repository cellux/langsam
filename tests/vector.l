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

; VectorSlice
(assert (= (type (slice [1 2 3] 0 2)) VectorSlice))
(assert (= (Vector (VectorSlice [1 2 3])) [1 2 3]))

(let [s (slice [1 2 3 4] 1 3)]
  (assert (= (len s) 2))
  (assert (= (get s 0) 2))
  (assert (= (get s 1) 3))
  (assert (= (get s -1) 3))
  (assert (= (s 0) 2))
  (assert (throws? '(get . "VectorSlice index 2 out of range (0..1)")
                   (get s 2)))
  (assert (throws? '(put . "VectorSlice index 2 out of range (0..1)")
                   (put s 2 99))))

(assert (= (Vector (slice [1 2 3 4] 2)) [3 4]))
(assert (= (Vector (slice [1 2 3 4] -3 -1)) [2 3]))

; slices share the underlying vector storage
(let [v [10 20 30 40]
      s (slice v 1 3)]
  (put s 0 99)
  (assert (= (get v 1) 99))
  (put v 2 77)
  (assert (= (get s 1) 77)))

; reslicing keeps sharing storage with the base vector
(let [v [1 2 3 4 5]
      s1 (slice v 1 5)
      s2 (slice s1 1 3)]
  (assert (= (Vector s2) [3 4]))
  (put s2 0 30)
  (assert (= v [1 2 30 4 5])))

; VectorSliceIterator
(let [s (slice '[a b c] 1 3)
      it (iter s)
      last-it nil
      index 0]
  (while it
    (assert (= (type it) VectorSliceIterator))
    (setq last-it it)
    (assert (= @it (get s index)))
    (setq it (it))
    (incq index))
  (assert (throws? '(deref . "attempt to deref consumed VectorSliceIterator")
                   @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed VectorSliceIterator")
                   (last-it))))

; slicing errors
(assert (throws? '(slice . "slice expects Vector, VectorSlice, String, or StringSlice, got Map")
                 (slice {} 0 0)))
(assert (throws? '(slice . "slice start 4 out of range (0..3)")
                 (slice [1 2 3] 4 4)))
(assert (throws? '(slice . "slice end 4 out of range (0..3)")
                 (slice [1 2 3] 0 4)))
(assert (throws? '(slice . "slice end 1 is before start 2")
                 (slice [1 2 3] 2 1)))
