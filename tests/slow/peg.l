(import peg)
(import os)

(let [compiled (peg/compile "abc")]
  (assert (= (:kind compiled) :compiled-peg))
  (assert (= (:source compiled) "abc"))
  (assert (= (:grammar compiled) nil))
  (assert (= (:root compiled) "abc"))
  (assert (= (:nodes compiled) {}))
  (assert (= (:rules compiled) {}))
  (assert (map? (:defaults compiled)))
  (assert (= (get (:defaults compiled) :d) '(range "09")))
  (assert (= (get (:defaults compiled) :w) '(or :a :d)))
  (assert (= (get (:defaults compiled) :d+) '(some :d)))
  (assert (= (get (:defaults compiled) :d*) '(any :d))))

(let [grammar {:main '(and "a" "b")
               :ws :s}
      compiled (peg/compile grammar)]
  (assert (= (:source compiled) grammar))
  (assert (= (:grammar compiled) grammar))
  (assert (= (:root compiled) '(and "a" "b")))
  (assert (= (:rules compiled) grammar)))

(assert (throws? '(peg . "grammar map requires :main rule")
                 (peg/compile {:ws :s})))

(let [compiled (peg/compile "xyz")]
  (assert (= (peg/compile compiled) compiled)))

; primitive patterns
(assert (= (peg/match "abc" "abcdef") []))
(assert (= (peg/match "abd" "abcdef") nil))

(assert (= (peg/match true "x") []))
(assert (= (peg/match true "") nil))
(assert (= (peg/match false "x") nil))

(assert (= (peg/match 3 "abcdef") []))
(assert (= (peg/match 0 "abcdef") []))
(assert (= (peg/match 7 "abcdef") nil))
(assert (= (peg/match -1 "") []))
(assert (= (peg/match -1 "a") nil))
(assert (= (peg/match -2 "a") []))

(assert (= (peg/match '(set "abc") "bcd") []))
(assert (= (peg/match '(set "abc") "zcd") nil))
(assert (= (peg/match '(range "az") "m") []))
(assert (= (peg/match '(range "az") "M") nil))

; core combinators and aliases
(assert (= (peg/match '(and "ab" "cd") "abcdef") []))
(assert (= (peg/match '(* "ab" "cd") "abcdef") []))
(assert (= (peg/match '(or "ab" "cd") "cdef") []))
(assert (= (peg/match '(+ "ab" "cd") "abef") []))
(assert (throws? '(peg . "unsupported PEG operator: sequence")
                 (peg/match '(sequence "ab" "cd") "abcdef")))
(assert (throws? '(peg . "unsupported PEG operator: choice")
                 (peg/match '(choice "ab" "cd") "cdef")))

(assert (= (peg/match '(any "ab") "ababx") []))
(assert (= (peg/match '(some "ab") "ababx") []))
(assert (= (peg/match '(some "ab") "xabab") nil))

(assert (= (peg/match '(repeat 2 "ab") "ababx") []))
(assert (= (peg/match '(2 "ab") "ababx") []))
(assert (= (peg/match '(at-least 1 "ab") "ababx") []))
(assert (= (peg/match '(at-most 2 "ab") "ababab") []))
(assert (= (peg/match '(between 1 2 "ab") "ababab") []))

(assert (= (peg/match '(not "ab") "cd") []))
(assert (= (peg/match '(not "ab") "ab") nil))
(assert (throws? '(peg . "unsupported PEG operator: !")
                 (peg/match '(! "ab") "cd")))

(assert (= (peg/match '(look "ab") "abcd") []))
(assert (= (peg/match '(look 2 "cd") "abcd") []))
(assert (= (peg/match '(> 2 "cd") "abcd") []))
(assert (= (peg/match '(look 1 "cd") "abcd") nil))

(assert (= (peg/match '(if "ab" "ab") "abcd") []))
(assert (= (peg/match '(if "xy" "ab") "abcd") nil))
(assert (= (peg/match '(if-not "xy" "ab") "abcd") []))
(assert (= (peg/match '(if-not "ab" "ab") "abcd") nil))

(assert (= (peg/match '(opt "ab") "abcd") []))
(assert (= (peg/match '(? "ab") "zz") []))

; default aliases
(assert (= (peg/match :d "1") []))
(assert (= (peg/match :D "a") []))
(assert (= (peg/match :d+ "123x") []))
(assert (= (peg/match :d* "x") []))
(assert (= (peg/match :w "A") []))
(assert (= (peg/match :w "_") nil))

; grammar refs and recursion
(assert (= (peg/match {:main :ab
                       :ab '(and "a" "b")}
                      "ab")
           []))
(assert (= (peg/match {:main 'ab
                       'ab '(and "a" "b")}
                      "ab")
           []))

; recursive grammars should evaluate without hitting evaluator recursion depth
(assert (= (peg/match {:main :s
                       :s '(or (and "(" :s ")" :s) "")}
                      "(()())")
           []))
(assert (= (peg/match {:main :expr
                       :expr '(or (and :expr "a") "")}
                      "aaaa")
           []))

; captures
(assert (= (peg/match '(capture "ab") "abz") ["ab"]))
(assert (= (peg/match '(<- "ab") "abz") ["ab"]))
(assert (= (peg/match '(quote "ab") "abz") ["ab"]))
(assert (= (peg/match '(group (and (capture "a") (capture "b"))) "ab")
           [["a" "b"]]))
(assert (= (peg/match '(or (and (capture "a") "x")
                               (and (capture "a") "b"))
                      "ab")
           ["a"]))
(assert (= (peg/match '(and (position) "x") "x\ny") [0]))
(assert (= (peg/match '(and (line) (column) "x") "x\ny") [1 1]))
(assert (= (peg/match '(and (line) (column) "y") "x\ny" 2) [2 1]))
(assert (= (peg/match '(and (line :loc) (column :loc) "y") "x\ny" 2) [2 1]))
(assert (= (peg/match '(and (capture "ab" :x) (backref :x)) "ab") ["ab" "ab"]))
(assert (= (peg/match '(and (capture "ab" :x) (-> :x :y)) "ab") ["ab" "ab"]))
(assert (= (peg/match '(and (capture "ab" :x) (backmatch :x)) "abab") ["ab"]))
(assert (= (peg/match '(and (capture "ab") (backmatch)) "abab") ["ab"]))
(assert (= (peg/match '(drop (capture "ab")) "ab") []))
(assert (= (peg/match '(and (capture "ab" :x)
                                (unref (capture "cd" :x) :x)
                                (backref :x))
                      "abcd")
           ["ab" "cd" "ab"]))
(assert (= (peg/match '(and (capture "ab" :x)
                                (drop (capture "cd" :x))
                                (backref :x))
                      "abcd")
           ["ab" "ab"]))

; stream behavior for anchored match
(let [s (stream "abcdef")]
  (assert (= (peg/match "abc" s) []))
  (assert (= (s.read) "def")))

(let [s (stream "abcdef")]
  (assert (= (peg/match "xy" s) nil))
  (assert (= (s.read) "abcdef")))

(let [s (stream "abcdef")]
  (assert (= (peg/match "cd" s 2) []))
  (assert (= (s.read) "ef")))

; stream behavior for anchored match (file stream)
(let [path "/tmp/langsam-peg-match-filestream.txt"]
  (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
    (os/write f "abcdef")
    (os/close f))
  (let [s (stream (os/open path os/O_RDONLY))]
    (assert (= (peg/match "abc" s) []))
    (assert (= (s.read) "def"))
    (s.close))
  (os/unlink path))

; find semantics (consumes scanned input)
(assert (= (peg/find "abc" "xxabczz") []))
(assert (= (peg/find "abc" "xxabzz") nil))

(let [s (stream "xxabczz")]
  (assert (= (peg/find "abc" s) []))
  (assert (= (s.read) "zz")))

(let [s (stream "abcabc")]
  (assert (= (peg/find "ab" s 2) []))
  (assert (= (s.read) "c")))

(let [s (stream "abcdef")]
  (assert (= (peg/find "zz" s) nil))
  (assert (s.eof)))

(let [s (stream "abcdef")]
  (assert (= (peg/find "ab" s 99) nil))
  (assert (s.eof)))

(let [s (stream "abc")]
  (assert (= (peg/find 0 s 2) []))
  (assert (= (s.read) "c")))

(assert (= (peg/find '(and "xx" (capture "ab")) "xxabzz") ["ab"]))

; find semantics (consumes scanned input, file stream)
(let [path "/tmp/langsam-peg-find-filestream.txt"]
  (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
    (os/write f "xxabczz")
    (os/close f))
  (let [s (stream (os/open path os/O_RDONLY))]
    (assert (= (peg/find "abc" s) []))
    (assert (= (s.read) "zz"))
    (s.close))
  (os/unlink path))

; find-all semantics (consumes to EOF)
(assert (= (peg/find-all "ab" "xxabyyabz") [[] []]))
(assert (= (peg/find-all "ab" "xxabyyabz" 3) [[]]))
(assert (= (peg/find-all "zz" "abcdef") []))
(assert (= (len (peg/find-all 0 "abc")) 4))
(assert (= (peg/find-all '(capture "ab") "abxxab") [["ab"] ["ab"]]))

(let [s (stream "xxabyyabz")]
  (assert (= (peg/find-all "ab" s) [[] []]))
  (assert (s.eof)))

; find-all semantics (consumes to EOF, file stream)
(let [path "/tmp/langsam-peg-findall-filestream.txt"]
  (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
    (os/write f "xxabyyabz")
    (os/close f))
  (let [s (stream (os/open path os/O_RDONLY))]
    (assert (= (peg/find-all "ab" s) [[] []]))
    (assert (s.eof))
    (s.close))
  (os/unlink path))

; replace semantics (consumes to EOF)
(assert (= (peg/replace "ab" "X" "zzabyyab") "zzXyyX"))
(assert (= (peg/replace "ab" "X" "ab--ab" 3) "ab--X"))
(assert (= (peg/replace "zz" "X" "abcdef") "abcdef"))
(assert (= (peg/replace "zz" "X" "abc" 99) "abc"))
(assert (= (peg/replace 0 "_" "ab") "_a_b_"))

(let [s (stream "zzabyyab")]
  (assert (= (peg/replace "ab" "X" s) "zzXyyX"))
  (assert (s.eof)))

; replace semantics (consumes to EOF, file stream)
(let [path "/tmp/langsam-peg-replace-filestream.txt"]
  (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
    (os/write f "zzabyyab")
    (os/close f))
  (let [s (stream (os/open path os/O_RDONLY))]
    (assert (= (peg/replace "ab" "X" s) "zzXyyX"))
    (assert (s.eof))
    (s.close))
  (os/unlink path))

; replacef semantics (consumes to EOF)
(assert (= (peg/replacef "ab" (fn [m] (+ "[" m "]")) "abxxab") "[ab]xx[ab]"))
(assert (= (peg/replacef "ab" (fn [m] (len m)) "abxxab") "2xx2"))

(let [s (stream "abxxab")]
  (assert (= (peg/replacef "ab" (fn [m] (+ "<" m ">")) s) "<ab>xx<ab>"))
  (assert (s.eof)))

; replacef semantics (consumes to EOF, file stream)
(let [path "/tmp/langsam-peg-replacef-filestream.txt"]
  (let [f (os/open path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
    (os/write f "abxxab")
    (os/close f))
  (let [s (stream (os/open path os/O_RDONLY))]
    (assert (= (peg/replacef "ab" (fn [m] (+ "<" m ">")) s) "<ab>xx<ab>"))
    (assert (s.eof))
    (s.close))
  (os/unlink path))

; additional combinators
(assert (= (peg/match '(and (to "ab") "ab") "xxabyy") []))
(assert (= (peg/match '(to "ab") "xxxx") nil))
(assert (= (peg/match '(thru "ab") "xxabyy") []))
(assert (= (peg/match '(sub "abcd" (and "ab" "c")) "abcde") []))
(assert (= (peg/match '(sub "ab" "abc") "abc") nil))
(assert (= (peg/match '(split "," "ab") "ab,ab,ab!") []))
(assert (= (peg/match '(split "," "ab") "zzz") []))
(assert (= (peg/match '(split "," (capture "ab")) "ab,ab") ["ab" "ab"]))

; additional capture operators
(assert (= (peg/match '(replace (and (capture "a") (capture "b")) "X") "ab") ["X"]))
(assert (= (peg/match '(/ (capture "ab") "X") "ab") ["X"]))
(assert (= (peg/match '(replace (or (capture "a") (capture "b")) {"a" 1 "b" 2}) "b") [2]))
(let [f (fn [x y] (+ x y))]
  (assert (= (peg/match `(replace (and (capture "a") (capture "b")) ,f) "ab") ["ab"])))

(assert (= (peg/match '(and (constant 42) "a") "a") [42]))
(assert (= (peg/match '(and (constant "x" :k) (backref :k)) "") ["x" "x"]))
(assert (= (peg/match '(and (argument 0) "a") "a" 0 99) [99]))
(assert (= (peg/match '(argument 3) "" 0 :x) [nil]))

(assert (= (peg/match '(accumulate (and (capture "a") (capture "b"))) "ab") [["a" "b"]]))
(assert (= (peg/match '(% (and (capture "a") (capture "b"))) "ab") [["a" "b"]]))
(let [f (fn [x y] (+ x y))]
  (assert (= (peg/match `(cmt (and (capture "a") (capture "b")) ,f) "ab") ["ab"])))
(assert (= (peg/match '(only-tags (and (capture "a") (capture "b" :x) (capture "c"))) "abc")
           ["b"]))
(assert (= (peg/match '(nth 1 (and (capture "a") (capture "b") (capture "c"))) "abc")
           ["b"]))
(assert (= (peg/match '(nth -1 (and (capture "a") (capture "b"))) "ab")
           ["b"]))

(assert (= (peg/match '(uint 2) "\x34\x12") [0x1234]))
(assert (= (peg/match '(uint-be 2) "\x12\x34") [0x1234]))
(assert (= (peg/match '(int 2) "\xFE\xFF") [-2]))
(assert (= (peg/match '(int-be 2) "\xFF\xFE") [-2]))

(let [s (stream "\x03abcZ")]
  (assert (= (peg/match '(lenprefix 1 (and "ab")) s) []))
  (assert (= (s.read) "cZ")))
(assert (= (peg/match '(lenprefix 1 "a") "\x05ab") nil))

(assert (= (peg/match '(number "123") "123") [123]))
(assert (= (peg/match '(number "ff" 16) "ff") [255]))
(assert (= (peg/match '(number "3.5") "3.5") [3.5]))

(assert (throws? '(peg . "parse error at line 1 column 1")
                 (peg/match '(error) "abc")))
(assert (throws? '(peg . "parse error at line 1 column 1: boom")
                 (peg/match '(error (capture "boom")) "boom")))

(assert (= (peg/match '(and (capture "ab") (capture "cd" :x) (backmatch)) "abcdab")
           ["ab" "cd"]))
