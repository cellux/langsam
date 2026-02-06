(let [result (catch [e (str "payload:" e)]
               (let [x (+ 1 2)]
                 (throw x)))]
  (assert (= result "payload:3")))

(let [f 5
      result (try
              (swapq f + 3)
              (swapq f / 0)
              (%catch (cons 'div message)
                      (str "caught exception with message: " message))
              (%finally (swapq f * 4)))]
  (assert (= f 32))
  (assert (= result "caught exception with message: division by zero")))

(let [thrown-payload :foo
      caught-payload (catch [x x] (Exception thrown-payload))]
  (assert (= caught-payload thrown-payload)))
