(let [x 5]
  (assert (= x 5))
  (do
    (def x 6)
    (def [y z] [x (* 2 x)])
    (assert (= x 6))
    (assert (= y 6))
    (assert (= z 12)))
  (assert (= x 6)))

(let [x 10]
  (def x 11 "Update x.")
  (assert (= x 11)))

(assert (throws? '(type . "argument doc is Integer, expected String")
                 (def x 1 2)))
(assert (throws? '(syntax . "def expects (def lhs value &opt docstring), got extra form(s)")
                 (def x 1 "doc" "extra")))
