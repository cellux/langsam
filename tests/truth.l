(let [truthy-values [true
                     0 1 -1 0.0 1.0 -1.0
                     "" "foo"
                     'foo :foo %foo
                     [] [nil]
                     {} {nil nil}
                     '(nil) '(nil . nil)
                     let do unless map]
      falsey-values [false nil]]
  (for [v truthy-values] (assert (= (Boolean v) true)))
  (for [v falsey-values] (assert (= (Boolean v) false))))
