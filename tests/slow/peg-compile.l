(import peg)

; compile-time validation: reference checks
(assert (throws? '(peg . "unknown PEG reference: :missing")
                 (peg/compile :missing)))

(assert (throws? '(peg . "unknown PEG reference: :missing")
                 (peg/compile {:main :start
                               :start '(and :missing "a")})))

; compile-time validation: arity checks
(assert (throws? '(peg . "any expects 1 arg, got 0")
                 (peg/compile '(any))))

(assert (throws? '(peg . "set expects 1 arg, got 0")
                 (peg/compile '(set))))

; compile-time validation: type/value checks
(assert (throws? '(peg . "look offset should be Integer, got String")
                 (peg/compile '(look "1" "a"))))

(assert (throws? '(peg . "repeat count should be Integer, got String")
                 (peg/compile '(repeat "2" "a"))))

(assert (throws? '(peg . "repeat count must be non-negative, got -1")
                 (peg/compile '(repeat -1 "a"))))

(assert (throws? '(peg . "between expects min <= max, got 2 > 1")
                 (peg/compile '(between 2 1 "a"))))

(assert (throws? '(peg . "range entries must have length 2, got 3")
                 (peg/compile '(range "abc"))))

(assert (throws? '(peg . "set expects String charset, got Integer")
                 (peg/compile '(set 1))))

(assert (throws? '(peg . "argument index should be non-negative, got -1")
                 (peg/compile '(argument -1))))

(assert (throws? '(peg . "number base should be Integer, got String")
                 (peg/compile '(number "ff" "16"))))

(assert (throws? '(peg . "lenprefix width should be Integer, got String")
                 (peg/compile '(lenprefix "1" "a"))))

(assert (throws? '(peg . "integer width should be positive, got 0")
                 (peg/compile '(uint 0))))

; compile-time validation should remain iterative and not hit eval depth.
(let [depth 1024
      pattern 0
      i 0]
  (while (< i depth)
    (setq pattern `(and 0 ,pattern))
    (incq i))
  (let [compiled (peg/compile pattern)]
    (assert (= (:kind compiled) :compiled-peg))))
