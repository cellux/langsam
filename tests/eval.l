(assert (= (eval (+ 5 3)) 8))

(let [env (let [x 5 y 8]
            (curlet))
      f (fn [x] (eval '(+ x y) env))]
  (assert (= (env 'x) 5))
  (assert (= (env 'y) 8))
  (assert (= (f 0) 13)))
