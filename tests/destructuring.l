(let [dtest (fn [pattern value expected-result]
              (let [actual-result (destructure pattern value)]
                (assert (= actual-result expected-result))))]
  (dtest 'a 5 {'a 5})
  (dtest '[a b c] [1 2 3] {'a 1 'b 2 'c 3})
  (dtest '[a b & rest] [1 2 3 4 5 6] {'a 1 'b 2 'rest '(3 4 5 6)})
  (dtest '[& rest] [1 2 3 4 5 6] {'rest '(1 2 3 4 5 6)})
  (dtest '[a b & [c & rest]] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)})
  (dtest '[a [b c] & rest] [1 '(2 3) 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)})
  (dtest '[a b &opt c (d :foo)] [1 2] {'a 1 'b 2 'c nil 'd :foo})
  (dtest '[a b &opt c (d :foo)] [1 2 3] {'a 1 'b 2 'c 3 'd :foo})
  (dtest '[a b &opt c (d :foo)] [1 2 3 4] {'a 1 'b 2 'c 3 'd 4})
  (dtest '[a b &opt c (d :foo)] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'd 4})
  (dtest '[a b &opt c (d :foo) & rest] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'd 4 'rest '(5 6)})
  (dtest '[a b &opt (c 7) (d (+ c 4))] [1 2] {'a 1 'b 2 'c 7 'd 11})
  (dtest '[a b &opt (c 'foo)] [1 2] {'a 1 'b 2 'c 'foo})
  (dtest '{a :a b :b} {:a 1 :b 2 :c 3} {'a 1 'b 2})
  (dtest '{:keys [a c] b :b} {:a 1 :b 2 :c 3} {'a 1 'c 3 'b 2}))
