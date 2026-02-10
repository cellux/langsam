(assert (nil? (nreverse nil)))

(assert (= (nreverse '(1 2 3)) '(3 2 1)))

(assert (= (group-by car '((foo bar) (bar goo) (foo baz)))
           {'foo '((foo baz) (foo bar)) 'bar '((bar goo))}))

(assert (= (separate (fn [x] (> x 3)) [1 2 3 4 5 6 7])
           ['(4 5 6 7) '(1 2 3)]))

(assert (= (separate even? [1 2 3 4 5 6 7])
           ['(2 4 6) '(1 3 5 7)]))

(assert (= (->> [3 4 5 6 7 8 9]
             (filter even?)
             (map (fn [x] (/ x 2))))
          '(2 3 4)))

(assert (= (catv) []))
(assert (= (catv [1 2] '(3 4) 5) [1 2 3 4 5]))
(assert (= (catv [1 [2 3]] '(4)) [1 [2 3] 4]))
(assert (= (catv nil [1] nil '(2)) [1 2]))

(assert (iterable? nil))
(assert (iterable? '()))
(assert (iterable? []))
(assert (iterable? {}))
(assert (iterable? '(1 2 3)))
(assert (iterable? [1 2 3]))
(assert (iterable? {:a 1}))

(assert (not (iterable? 0)))
(assert (not (iterable? 1.5)))
(assert (not (iterable? "")))
(assert (not (iterable? "abc")))
(assert (not (iterable? true)))

; drop/tail helpers
(assert (= (drop 0 [1 2 3]) [1 2 3]))
(assert (= (drop 2 [1 2 3 4]) [3 4]))
(assert (= (drop 10 [1 2]) []))
(assert (= (drop 2 '(1 2 3 4)) '(3 4)))
(assert (= (drop 10 '(1 2)) '()))
(assert (= (drop 1 nil) nil))
(assert (= (tail [1 2 3]) [2 3]))
(assert (= (tail '(1 2 3)) '(2 3)))
(assert (= (tail nil) nil))

; membership and quantifiers
(assert (member? 3 [1 2 3]))
(assert (not (member? 4 [1 2 3])))
(assert (member? :a '(:a :b)))
(assert (not (member? :c nil)))

(assert (every? integer? [1 2 3]))
(assert (not (every? even? [2 3 4])))
(assert (every? even? []))
(assert (any? even? [1 3 4]))
(assert (not (any? even? [1 3 5])))
(assert (not (any? even? [])))

; range iterator
(assert (= (map (fn [x] x) (range 10))
           '(0 1 2 3 4 5 6 7 8 9)))
(assert (= (map (fn [x] x) (range 5 10))
           '(5 6 7 8 9)))
(assert (= (map (fn [x] x) (range 2 10 3))
           '(2 5 8)))
(assert (= (map (fn [x] x) (range 10 4 -2))
           '(10 8 6)))

(assert (= (range 0) nil))
(assert (= (range 5 5) nil))
(assert (= (range 5 0) nil))
(assert (= (range 0 5 -1) nil))

(assert (throws? '(range . "range step cannot be 0")
                 (range 0 10 0)))
(assert (throws? '(range . "range expects 1 to 3 args, got 0")
                 (range)))
(assert (throws? '(range . "range expects Integer arguments, got Integer String Integer")
                 (range 0 "5")))

(let [it (range 1)
      last-it nil
      items nil]
  (while it
    (setq last-it it)
    (setq items (cons @it items))
    (setq it (next it)))
  (assert (= items '(0)))
  (assert (throws? '(deref . "attempt to deref consumed RangeIterator") @last-it))
  (assert (throws? '(invoke . "attempt to advance consumed RangeIterator") (last-it))))
