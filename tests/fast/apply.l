(assert (= (apply + 3 4 nil) 7))
(assert (= (apply + 3 4 '(1 6)) 14))
(assert (= (apply + 3 4 [1 6]) 14))
(assert (throws? '(apply . "rest arg should be iterable, got Integer") (apply + 3 4) 7))
(let [rest '(3 4 5)]
  (assert (= (apply + 1 2 rest) 15)))
