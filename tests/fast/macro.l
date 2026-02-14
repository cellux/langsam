(let [m (macro m [x] `(+ ,x 1))
      w (macro w [y] `(m (* ,y 2)))
      z 5]
  (assert (macro? m))
  (assert (macro-form? '(m 3)))
  (assert (= (repr m) "<Macro:m>"))
  (assert (= (:body m) '((quasiquote (+ (unquote x) 1)))))
  (assert (= (m (* z 3)) 16))
  (assert (= (w (* z 3)) 31))
  (assert (= (macroexpand-1 '(m (* z 3))) '(+ (* z 3) 1)))
  (assert (= (macroexpand-1 '(w (* z 3))) '(m (* (* z 3) 2))))
  (assert (= (macroexpand '(m (* z 3))) '(+ (* z 3) 1)))
  (assert (= (macroexpand '(w (* z 3))) '(+ (* (* z 3) 2) 1)))
  (assert (= (macroexpand-all '(m (* z 3))) '(+ (* z 3) 1)))
  (assert (= (macroexpand-all '(w (* z 3))) '(+ (* (* z 3) 2) 1)))
  (assert (= (macroexpand-all '(w (* z (m 3)))) '(+ (* (* z (+ 3 1)) 2) 1))))

(let [m (macro m [x] x)
      w (macro w [x] `(m ,x))]
  (assert (= (macroexpand-all 'a) 'a))
  (assert (= (macroexpand '(m 12)) 12))
  (assert (= (macroexpand '(w 12)) 12))
  (assert (= (macroexpand-all '(w 12)) 12))
  (assert (= (macroexpand-all '(m (w 5))) 5))
  (assert (= (macroexpand-all '(w (m 5))) 5))
  (assert (= (macroexpand-all '[:a (w 5)]) '[:a 5]))
  (assert (= (macroexpand-all '(:a . (w 3))) '(:a w 3)))
  (assert (= (macroexpand-all '((w 3) . :a)) '(3 . :a)))
  (assert (= (macroexpand-all '(:a (w 3))) '(:a 3)))
  (assert (= (macroexpand-all '((w 3) :a)) '(3 :a)))
  (assert (= (macroexpand-all '{(w 3) :a (w 8) b}) '{3 :a 8 b}))
  (assert (= (macroexpand-all '{:a (w 3) b (w 8)}) '{:a 3 b 8}))
  (assert (= (macroexpand-all ''(:a (w 3))) ''(:a (w 3))))
  (assert (= (macroexpand-all ''(:a ,(w 3))) ''(:a ,(w 3))))
  (assert (= (macroexpand-all '`(:a (w 3)))
             '(quasiquote (:a (w 3)))))
  (assert (= (macroexpand-all '`(:a ,(w 3)))
             '(quasiquote (:a (unquote 3)))))
  (assert (= (macroexpand-all '`(:a ,@(w [1 (w :b) 3])))
             '(quasiquote (:a (unquote-splicing [1 :b 3])))))
  (assert (= (macroexpand-all '`(:a ,@(w [1 [(w :a) (w :b)] 3])))
             '(quasiquote (:a (unquote-splicing [1 [:a :b] 3])))))
  (assert (= (macroexpand-all '`(head (pair ,(w 3) tail)))
             '(quasiquote (head (pair (unquote 3) tail)))))
  (assert (= (macroexpand-all '(quasiquote (a (quasiquote (b (unquote (w 1)))))))
             '(quasiquote (a (quasiquote (b (unquote (w 1))))))))
  (assert (= (macro-form? (cons 'w :b)) false))
  (assert (= (macroexpand-all (cons 'w :b)) '(w . :b)))
  (assert (= (macroexpand-all '`m) '`m))
  (assert (= (macroexpand-all '`(m 1)) '`(m 1)))
  (assert (= (macroexpand-all '`(+ ,x 1)) '`(+ ,x 1)))
  (assert (= (macroexpand-all '(`(+ ,x 1))) '(`(+ ,x 1))))
  (assert (= (macroexpand-all '`(if 5 (throw '(foo . "bar"))) '`(if 5 (throw '(foo . "bar")))))))

(let [m (macro [] `(throw '(syntax . "foo")))]
  (assert (= (macroexpand-1 '(m)) '(throw '(syntax . "foo"))))
  (assert (= (macroexpand '(m)) '(throw '(syntax . "foo"))))
  (assert (= (macroexpand-all '(m)) '(throw '(syntax . "foo")))))

(assert (= (macroexpand '(cond-let [x y] x :none))
           '(if-let [x y] x :none)))

(assert (= (macroexpand '(cond a b c d :none))
           '(if a b (if c d :none))))

(assert (= (macroexpand '(cond-let [x nil] :a [y 5] y :none))
           '(if-let [x nil] :a (if-let [y 5] y :none))))

(assert (= (cond-match 5
             (String s) s
             (Integer x) (+ x 1)
             :none)
           6))

; Additional macro-expansion edge cases.
(assert (= (macro-form? '(+ 1 2)) false))
(assert (= (macro-form? '((fn [x] x) 1)) false))
(assert (= (macroexpand-1 '(+ 1 2)) '(+ 1 2)))
(assert (= (macroexpand '(+ 1 2)) '(+ 1 2)))
(assert (throws? '(syntax . "missing argument: form")
                 (macroexpand-1)))
(assert (throws? '(type . "argument env is Integer, expected Map")
                 (macroexpand-1 '(+ 1 2) 1)))

(let [env {'m (macro m [x] `(+ ,x 10))}
      m (macro m [x] `(- ,x 1))]
  (assert (= (macroexpand-1 '(m 5) env) '(+ 5 10)))
  (assert (= (macroexpand-1 '(m 5)) '(- 5 1))))

(let [m (macro [] (throw '(foo . "bar")))]
  (assert (throws? '(foo . "bar")
                   (macroexpand-1 '(m)))))

(assert (throws? '(foo . "bar")
                 (macroexpand-1 '((throw '(foo . "bar")) 1))))

(let [w (macro w [x] x)]
  (assert (= (macroexpand-all '`(a `(b ,@(w [1 2]))))
             '`(a `(b ,@(w [1 2]))))))

; Threading macros.
(assert (= (macroexpand '(-> x)) 'x))
(assert (= (macroexpand '(->> x)) 'x))
(assert (= (macroexpand '(-> x f)) '(f x)))
(assert (= (macroexpand '(->> x f)) '(f x)))
(assert (= (macroexpand '(-> x (f 1 2) g (h 3 4)))
           '(h (g (f x 1 2)) 3 4)))
(assert (= (macroexpand '(->> x (f 1 2) g (h 3 4)))
           '(h 3 4 (g (f 1 2 x)))))
(assert (= (-> 5 (+ 3) (* 2)) 16))
(assert (= (->> [1 2 3 4 5]
             (filter odd?)
             (map (fn [x] (* x 10))))
           '(10 30 50)))

; with-syms.
(assert (= (macroexpand '(with-syms [a] [a#]))
           '(let [a# (gensym 'a)] [a#])))

(let [a nil
      b nil]
  (with-syms [tmp]
    (assert (symbol? tmp#))
    (assert (not (= tmp# 'tmp)))
    (setq a tmp#))
  (with-syms [tmp]
    (setq b tmp#))
  (assert (not (= a b))))

(with-syms [x y]
  (assert (symbol? x#))
  (assert (symbol? y#))
  (assert (not (= x# y#))))

(let [tmp# :outer]
  (with-syms [tmp]
    (assert (symbol? tmp#))
    (assert (not (= tmp# :outer))))
  (assert (= tmp# :outer)))

(let [root (:funclet with-syms)
      old-thread (get root '->)
      broken-thread (macro -> [& rest]
                      `(throw '(optimize-fn . "broken ->")))]
  (put root '-> broken-thread)
  (assert (throws? '(optimize-fn . "broken ->")
                   (macroexpand '(with-syms [x] x))))
  (put root '-> old-thread))
