(assert (= (cond-let
             [(String s) 5] s
             [(Integer x) 5] (+ x 1)
             :none)
           6))

(assert (= (cond-let
             [(Float f) 5] f
             [(String s) 5] s
             :none)
           :none))

(assert (= (cond-let
             [(Integer x) 0x1234
              y (+ x 1)]
             [x y]
             :none)
           [0x1234 0x1235]))

(assert (= (cond-let
             [(Integer x) 7] x
             [(Integer y) (throw '(cond-let . "should not evaluate"))] y
             :none)
           7))

(assert (throws? '(cond-let . "no match")
                 (cond-let
                   [(String s) 5] s
                   [(Float f) 5] f)))
