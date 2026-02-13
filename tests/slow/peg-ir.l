(import peg)

(defn assert-match-ok
  [pattern input]
  (assert (= (peg/match pattern input)
             (peg/match (peg/compile pattern) input))))

(defn assert-find-ok
  [pattern input]
  (assert (= (peg/find pattern input)
             (peg/find (peg/compile pattern) input))))

(defn assert-find-all-ok
  [pattern input]
  (assert (= (peg/find-all pattern input)
             (peg/find-all (peg/compile pattern) input))))

(defn assert-replace-ok
  [pattern subst input]
  (assert (= (peg/replace pattern subst input)
             (peg/replace (peg/compile pattern) subst input))))

; compile IR shape and ref resolution
(let [compiled (peg/compile {:main '(and :word :d)
                             :word '(some :a)})
      nodes (:nodes compiled)
      n (len nodes)
      saw-ref false
      i 0]
  (assert (integer? (:root-id compiled)))
  (assert (< (:root-id compiled) n))
  (assert (contains? (:rule-ids compiled) :main))
  (assert (contains? (:rule-ids compiled) :word))
  (while (< i n)
    (let [node (get nodes i)]
      (if (= (:op node) :ref)
        (do
          (setq saw-ref true)
          (assert (integer? (:target node)))
          (assert (<= 0 (:target node)))
          (assert (< (:target node) n)))))
    (incq i))
  (assert saw-ref))

; compiled-vs-source checks now exercise the IR runtime path only.
(assert-match-ok "abc" "abcdef")
(assert-match-ok '(or "ab" "cd") "cdxx")
(assert-match-ok '(any "ab") "ababx")
(assert-match-ok '(between 1 2 "ab") "ababab")
(assert-match-ok '(look 2 "cd") "abcd")
(assert-match-ok '(split "," (capture "ab")) "ab,ab")
(assert-match-ok '(and (capture "ab" :x) (backmatch :x)) "abab")
(assert-match-ok '(and (line) (column) "y") "x\ny")
(assert-match-ok '(lenprefix 1 (and "ab")) "\x03abcZ")
(assert-match-ok {:main :s
                  :s '(or (and "(" :s ")" :s) "")}
                 "(()())")

(assert-find-ok '(capture "ab") "xxabyy")
(assert-find-all-ok '(capture "ab") "abxxab")
(assert-replace-ok "ab" "X" "zzabyyab")
(assert-replace-ok "ab" (fn [m] (+ "[" m "]")) "abxxab")
