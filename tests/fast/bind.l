(let [test-bind (fn [pattern value expected-env]
                  (let [env (sublet (curlet))]
                    (bind env pattern value)
                    (assert (= env expected-env))))]
  (test-bind 'a 5 {'a 5})
  (test-bind '[a b c] [1 2 3] {'a 1 'b 2 'c 3})
  (test-bind '[a b c] '(1 2 3) {'a 1 'b 2 'c 3})
  (test-bind '[[a b]] '(((1) [2])) {'a '(1) 'b [2]})
  (test-bind '[a b & rest] [1 2 3 4 5 6] {'a 1 'b 2 'rest '(3 4 5 6)})
  (test-bind '[& rest] [1 2 3 4 5 6] {'rest '(1 2 3 4 5 6)})
  (test-bind '[a b & [c & rest]] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)})
  (test-bind '[a [b c] & rest] [1 '(2 3) 4 5 6] {'a 1 'b 2 'c 3 'rest '(4 5 6)})
  (test-bind '[a b &opt c (d :foo)] [1 2] {'a 1 'b 2 'c nil 'd :foo})
  (test-bind '[&opt ((Integer n) 7)] [] {'n 7})
  (test-bind '[&opt ((Integer n) 7)] [3] {'n 3})
  (assert (throws? '(bind . "type mismatch: (Integer n)")
                   (test-bind '[&opt ((Integer n) 7)] [1.5] {})))
  (test-bind '[a b &opt c (d :foo)] [1 2 3] {'a 1 'b 2 'c 3 'd :foo})
  (test-bind '[a b &opt c (d :foo)] [1 2 3 4] {'a 1 'b 2 'c 3 'd 4})
  (test-bind '[a b &opt c (d :foo)] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'd 4})
  (test-bind '[a b &opt c (d :foo) & rest] [1 2 3 4 5 6] {'a 1 'b 2 'c 3 'd 4 'rest '(5 6)})
  (test-bind '[a b &opt (c 7) (d (+ c 4))] [1 2] {'a 1 'b 2 'c 7 'd 11})
  (test-bind '[a b &opt (c 'foo)] [1 2] {'a 1 'b 2 'c 'foo})
  (test-bind '[a b &opt (c :foo c-set?)] [1 2] {'a 1 'b 2 'c :foo 'c-set? false})
  (test-bind '[a b &opt (c :foo c-set?)] [1 2 nil] {'a 1 'b 2 'c nil 'c-set? true})
  (test-bind '{a :a b :b} {:a 1 :b 2 :c 3} {'a 1 'b 2})
  (test-bind '{:keys [a c] b :b} {:a 1 :b 2 :c 3} {'a 1 'c 3 'b 2})
  (test-bind '{:keys [a (b 2) c]} {:a 1 :c 3} {'a 1 'c 3 'b 2})
  (test-bind '{:keys [(a 2)]} {:a nil} {'a nil})
  (test-bind '[1 foo 2] [1 7 2] {'foo 7})
  (test-bind '[:bar foo :baz] [:bar 7 :baz] {'foo 7})
  (test-bind '['bar foo 'baz] ['bar 7 'baz] {'foo 7})
  (test-bind '['5 foo '3] [5 7 3] {'foo 7})
  (test-bind '`a 'a {})
  (test-bind '`,a 5 {'a 5})
  (test-bind '`(a b ,c d) '(a b foo d) {'c 'foo})
  (test-bind '`(a b ,[x y & rest] d) '(a b (1 2 3 4) d) {'x 1 'y 2 'rest '(3 4)})
  (test-bind '`(a b ,@[x y & rest]) '(a b 1 2 3 4) {'x 1 'y 2 'rest '(3 4)})
  (test-bind '`(a b ,@rest) '(a b 1 2 3 4) {'rest '(1 2 3 4)})
  (test-bind '(cons a b) '(:x . :y) {'a :x 'b :y})
  (test-bind '(cons a b c) '(:x :y . :z) {'a :x 'b :y 'c :z})
  (test-bind '(cons a b c) '(:x :y :z) {'a :x 'b :y 'c '(:z)})
  (assert (throws? '(bind . "pattern failed: (and (pred float?) f) value=5")
                   (test-bind '(and (pred float?) f) 5 {})))
  (test-bind '(and (pred float?) f) 5.0 {'f 5.0})
  (test-bind '(and (or (pred float?) (pred string?)) f) 5.0 {'f 5.0})
  (test-bind '(and (or (pred float?) (pred string?)) f) "foo" {'f "foo"})
  (assert (throws? '(bind . "pattern failed: (and (or (pred float?) (pred string?)) f) value=5")
                   (test-bind '(and (or (pred float?) (pred string?)) f) 5 {})))
  (assert (throws? '(syntax . "invalid cons pattern in bind form: (when (> f 3))")
                   (bind (sublet (curlet)) '(when (> f 3)) 4)))
  (assert (throws? '(syntax . "& must be followed by a single form")
                   (bind (sublet (curlet)) '[&] [])))
  (assert (throws? '(syntax . "& must be followed by a single form")
                   (bind (sublet (curlet)) '[& a b] [])))
  (assert (throws? '(syntax . "&opt must be followed by at least one parameter")
                   (bind (sublet (curlet)) '[&opt] [])))
  (assert (throws? '(syntax . "&opt parameter with default value should look like (pattern default) or (pattern default symsetp), got (x 1 y z)")
                   (bind (sublet (curlet)) '[&opt (x 1 y z)] [])))
  (test-bind '(and f (guard (> f 3))) 4 {'f 4})
  (assert (throws? '(bind . "pattern failed: (and f (guard (> f 3))) value=2")
                   (test-bind '(and f (guard (> f 3))) 2 {})))
  (test-bind '(String s) "foo" {'s "foo"})
  (test-bind '(or (Integer s) (Vector s)) 5 {'s 5})
  (test-bind '(or (Integer s) (Vector s)) [:foo] {'s [:foo]}))

(let [result nil]
  (for [x [1 2 3 4 5 6 7 8]]
    (when-match [(and (pred even?) z) x]
      (setq result (cons z result))))
  (assert (= result '(8 6 4 2))))

(let [items '(1 2 3 4)
      result nil]
  (while-match [[first second] items]
    (setq result (cons second result))
    (setq items (cdr items)))
  (assert (= result '(4 3 2))))

(let [x 5]
  (assert (= (if-match [(Integer y) x] (+ y 1) :none) 6))
  (assert (= (if-match [(String y) x] y :none) :none))
  (assert (= (if-let [y x] (+ y 1) :none) 6))
  (assert (= (if-let [y 0] y :none) 0))
  (assert (= (if-let [y false] y :none) :none))
  (assert (= (if-let [[a b] [1 2]] (+ a b) :none) 3)))

(let [Animal (make-class BindAnimal [:name String])
      Dog (make-class BindDog {:extends Animal} [:breed String])
      a (Animal {:name "spot"})
      d (Dog {:name "luna" :breed "mix"})]
  (assert (isa? 5 Integer))
  (assert (not (isa? 5 Float)))
  (assert (isa? d Dog))
  (assert (isa? d Animal))
  (assert (not (isa? a Dog)))
  (assert (= (if-match [(Dog x) d] (:breed x) :none) "mix"))
  (assert (= (if-match [(Animal x) d] (:name x) :none) "luna"))
  (assert (= (if-match [(Dog x) a] :bad :ok) :ok))
  (assert (= (let [(Animal x) d] (:name x)) "luna")))

(let [result nil]
  (for [x [0 false nil 2]]
    (when-let [v x]
      (setq result (cons v result))))
  (assert (= result '(2 0))))

(let [x 5]
  (assert (= x 5))
  (setq x 4)
  (assert (= x 4))
  (def x 7)
  (assert (= x 7))
  (let [x 6] (assert (= x 6)))
  (let [x 6 y 3]
    (assert (= x 6))
    (assert (= y 3)))
  (assert (= x 7)))

(let [x 5 y 6]
  (assert (= x 5))
  (assert (= y 6))
  (let [x 8]
    (assert (= x 8))
    (assert (= y 6))
    (setq x 7)
    (setq y 3)
    (assert (= x 7))
    (assert (= y 3))
    (let [addn (fn [x amount] (+ x amount))]
      (swapq x addn 9))
    (assert (= x 16))
    (incq x)
    (assert (= x 17)))
  (assert (= x 5))
  (assert (= y 3)))

(let [x 5]
  (assert (throws? '(case . "no match") (case x)))
  (assert (throws? '(case . "no match") (case x 3 :three 4 :four)))
  (assert (throws? '(case . "no match") (case x 3 :three (Float x) (+ x 1))))
  (assert (= (case x 3 :three 5 :five 4 :four) :five))
  (assert (= (case x 3 :three 4 :four :none) :none))
  (assert (= (case x 3 :three (Integer x) (+ x 1)) 6)))

(let [z 8
      f (fn [x &opt (y (+ z 1))] (cons x y))]
  (let [z 7]
    (assert (= (f 5) '(5 . 9)))))
