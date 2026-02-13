(import peg)

(defn strip-ir
  [compiled]
  (let [legacy (clone compiled)]
    (del legacy :ir-version)
    (del legacy :root-id)
    (del legacy :rule-ids)
    legacy))

(defn assert-match-parity
  [pattern input]
  (let [compiled (peg/compile pattern)
        legacy (strip-ir compiled)]
    (assert (= (peg/match compiled input)
               (peg/match legacy input)))))

(defn assert-find-parity
  [pattern input]
  (let [compiled (peg/compile pattern)
        legacy (strip-ir compiled)]
    (assert (= (peg/find compiled input)
               (peg/find legacy input)))))

(defn assert-find-all-parity
  [pattern input]
  (let [compiled (peg/compile pattern)
        legacy (strip-ir compiled)]
    (assert (= (peg/find-all compiled input)
               (peg/find-all legacy input)))))

(defn assert-replace-parity
  [pattern subst input]
  (let [compiled (peg/compile pattern)
        legacy (strip-ir compiled)]
    (assert (= (peg/replace compiled subst input)
               (peg/replace legacy subst input)))))

; compile IR shape and ref resolution
(let [compiled (peg/compile {:main '(and :word :d)
                             :word '(some :a)})
      nodes (:nodes compiled)
      n (len nodes)
      saw-ref false
      i 0]
  (assert (= (:ir-version compiled) 1))
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

; parity checks between IR runtime and legacy raw-form runtime
(assert-match-parity "abc" "abcdef")
(assert-match-parity '(or "ab" "cd") "cdxx")
(assert-match-parity '(any "ab") "ababx")
(assert-match-parity '(between 1 2 "ab") "ababab")
(assert-match-parity '(look 2 "cd") "abcd")
(assert-match-parity '(split "," (capture "ab")) "ab,ab")
(assert-match-parity '(and (capture "ab" :x) (backmatch :x)) "abab")
(assert-match-parity '(and (line) (column) "y") "x\ny")
(assert-match-parity '(lenprefix 1 (and "ab")) "\x03abcZ")
(assert-match-parity {:main :s
                      :s '(or (and "(" :s ")" :s) "")}
                     "(()())")

(assert-find-parity '(capture "ab") "xxabyy")
(assert-find-all-parity '(capture "ab") "abxxab")
(assert-replace-parity "ab" "X" "zzabyyab")
(assert-replace-parity "ab" (fn [m] (+ "[" m "]")) "abxxab")
