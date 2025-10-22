(let [foo (read-string "foo")]
  (assert (= foo 'foo))
  (assert (= (type foo) Symbol)))

(assert (= (read-string "   foo ;;; comment") 'foo))
(assert (= (read-string "   foo ;;; comment\n  bar") 'foo))

(let [foo (read-string ":foo")]
  (assert (= foo :foo))
  (assert (= (type foo) Keyword)))

(let [foo (read-string "%foo")]
  (assert (= foo %foo))
  (assert (= (type foo) Opword)))

(assert (= (read-string "foo/bar" '(get foo 'bar))))

(assert (= (read-string "foo.bar" '(cons foo 'bar))))

(let [num (read-string "123")]
  (assert (= num 123))
  (assert (= (type num) Integer)))

(assert (= (read-string "0xface") 64206))
(assert (= (read-string "0o644") 420))
(assert (= (read-string "0b01101001") 0x69))

(let [num (read-string "123.125")]
  (assert (= num 123.125))
  (assert (= (type num) Float)))

(let [num (read-string "-123")]
  (assert (= num (* 123 -1)))
  (assert (= (type num) Integer)))

(let [num (read-string "-123.125")]
  (assert (= num (* 123.125 -1)))
  (assert (= (type num) Float)))

(let [s (read-string "\"foo\"")]
  (assert (= s "foo"))
  (assert (= (type s) String)))

(let [s (read-string "\"foo\a\b\f\n\r\t\vbar\"")]
  (assert (= s "foo\a\b\f\n\r\t\vbar"))
  (assert (= (get s 3) 0x07))
  (assert (= (get s 4) 0x08))
  (assert (= (get s 5) 0x0c))
  (assert (= (get s 6) 0x0a))
  (assert (= (get s 7) 0x0d))
  (assert (= (get s 8) 0x09))
  (assert (= (get s 9) 0x0b))
  (assert (= (type s) String)))

(let [c (read-string "(foo . bar)")]
  (assert (= c (cons 'foo 'bar)))
  (assert (= (type c) Cons)))

(let [l (read-string "(foo bar baz)")]
  (assert (= l (cons 'foo (cons 'bar (cons 'baz nil)))))
  (assert (= l '(foo bar baz)))
  (assert (= (type l) Cons)))

(let [v (read-string "[foo bar baz]")]
  (assert (= v '[foo bar baz]))
  (assert (= v (Vector '(foo bar baz))))
  (assert (= (type v) Vector)))

(let [m (read-string "{:a 3 b 2 %c 8}")]
  (assert (= m {:a 3 'b 2 %c 8}))
  (assert (= m (Map '([:a 3] [b 2] [%c 8]))))
  (assert (= (type m) Map)))
