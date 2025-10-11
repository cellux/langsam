(let [dtest (fn [pattern value expected-result]
              (let [actual-result (destructure pattern value)]
                (assert (= actual-result expected-result))))]
  (dtest 'a 5 {'a 5})
  (dtest '[a b c] [1 2 3] {'a 1 'b 2 'c 3})
  (dtest '[a b & rest] [1 2 3 4 5 6] {'a 1 'b 2 'rest '(3 4 5 6)})
  (dtest '[& rest] [1 2 3 4 5 6] {'rest '(1 2 3 4 5 6)})
  (dtest '[a b & [c & rest]] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)})
  (dtest '[a [b c] & rest] [1 '(2 3) 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)}))
