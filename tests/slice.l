(assert (= (type (slice [1 2 3] 0 2)) Slice))
(assert (= (Vector (Slice [1 2 3])) [1 2 3]))

(let [s (slice [1 2 3 4] 1 3)]
  (assert (= (len s) 2))
  (assert (= (get s 0) 2))
  (assert (= (get s 1) 3))
  (assert (= (get s -1) 3))
  (assert (= (s 0) 2))
  (assert (throws? '(get . "Slice index 2 out of range (0..1)")
                   (get s 2)))
  (assert (throws? '(put . "Slice index 2 out of range (0..1)")
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

; SliceIterator
(let [s (slice '[a b c] 1 3)
      it (iter s)
      last-it nil
      index 0]
  (while it
    (assert (= (type it) SliceIterator))
    (setq last-it it)
    (assert (= @it (get s index)))
    (setq it (it))
    (incq index))
  (assert (throws? '(deref . "attempt to deref consumed SliceIterator")
                   @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed SliceIterator")
                   (last-it))))

; slicing errors
(assert (throws? '(slice . "slice expects Vector or Slice, got Map")
                 (slice {} 0 0)))
(assert (throws? '(slice . "slice start 4 out of range (0..3)")
                 (slice [1 2 3] 4 4)))
(assert (throws? '(slice . "slice end 4 out of range (0..3)")
                 (slice [1 2 3] 0 4)))
(assert (throws? '(slice . "slice end 1 is before start 2")
                 (slice [1 2 3] 2 1)))
