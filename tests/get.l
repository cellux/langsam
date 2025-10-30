(let [s "foo"]
  (assert (= (get s 1) 0x6f))
  (assert (throws? '(get . "attempt to index String with non-integer index of type Keyword")
                   (get s :zero)))
  (assert (throws? '(get . "String index 3 out of range (0..2)")
                   (get s 3))))

(let [c (cons :a :b)]
  (assert (= (get c 0) :a))
  (assert (= (get c 1) :b))
  (assert (throws? '(get . "attempt to index Cons with non-integer index of type Keyword")
                   (get c :car)))
  (assert (throws? '(get . "Cons index -2 out of range (0..1)")
                   (get c -2))))

(let [v [1 2 3]]
  (assert (= (get v 1) 2))
  (assert (throws? '(get . "attempt to index Vector with non-integer index of type Keyword")
                   (get v :zero)))
  (assert (throws? '(get . "Vector index 3 out of range (0..2)")
                   (get v 3))))

(let [m {:a 3 :b 5 :c 8}]
  (assert (= (get m :b) 5))
  (assert (= (m :b) 5))
  (assert (= (:b m) 5))
  (assert (= (:d m) nil))
  (assert (= (get m 'do) nil))
  (setproto m (curlet))
  (assert (= (get m 'do) do))
  (assert (= m/do do))
  (assert (= (type m/do) Function)))

(let [f (fn foo [x & rest] (apply str x rest))]
  (assert (= (get f :name) 'foo))
  (assert (= (get f :params) '[x & rest]))
  (assert (= (get f :body) '((apply str x rest))))
  (assert (= (get f :evalargs) true))
  (assert (= (get f :evalresult) false)))
