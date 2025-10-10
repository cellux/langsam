(let [dtest (fn [pattern value expected-result]
              (let [actual-result (destructure pattern value)]
                (assert (= actual-result expected-result))))]
  (dtest 'a 5 {'a 5}))
