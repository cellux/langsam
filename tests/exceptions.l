(let [result (catch [e (str "payload:" e)]
               (let [x (+ 1 2)]
                 (throw x)))]
  (assert (= result "payload:3")))
