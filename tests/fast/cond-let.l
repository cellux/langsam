(assert (= (cond-let
             [x (get {:x 5} :x)] (+ x 1)
             :none)
           6))

(assert (= (cond-let
             [x (get {:x nil} :x)] x
             [y 7] y
             :none)
           7))

(assert (= (cond-let
             [x 0
              y ""]
             [x y]
             :none)
           [0 ""]))

(assert (= (cond-let
             [[x y] [0x1234 0x1235]]
             [x y]
             :none)
           [0x1234 0x1235]))

(assert (throws? '(cond-let . "no match")
                 (cond-let
                   [x nil] x
                   [y false] y)))

(assert (= (cond-match
             5
             (String s) s
             (Integer x) (+ x 1)
             :none)
           6))

(assert (= (cond-match
             5
             (Float f) f
             (String s) s
             :none)
           :none))

(assert (= (cond-match
             [1 2]
             [x y] (+ x y)
             :none)
           3))

(assert (throws? '(cond-match . "no match")
                 (cond-match
                   5
                   (String s) s
                   (Float f) f)))
