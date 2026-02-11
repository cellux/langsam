(let [values [nil true false
              Nil Boolean String Symbol Map
              0 0.0 "foo" 'foo :foo %foo
              '(a b) '(a . b) [:a :b] {:a 1 :b 3 :c 8}
              let do unless map]
      grouped-by-hash (group-by hash values)]
  (assert (= (len grouped-by-hash) (len values)))
  (for [v values] (assert (= (hash v) (hash v)))))

(assert (= (hash 0.0) (hash -0.0)))

(let [nan1 (Float "nan")
      nan2 (Float "-nan")]
  (assert (= nan1 nan2))
  (assert (not= nan1 1.0))
  (assert (= (hash nan1) (hash nan2))))

(let [m1 (Map 2)
      m2 (Map 2)]
  (put m1 0 1)
  (put m1 4 2)
  (put m2 4 2)
  (put m2 0 1)
  (assert (= m1 m2))
  (assert (= (hash m1) (hash m2))))
