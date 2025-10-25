(let [values [nil true false
              Nil Boolean String Symbol Map
              0 0.0 "foo" 'foo :foo %foo
              '(a b) '(a . b) [:a :b] {:a 1 :b 3 :c 8}
              let do unless map]
      grouped-by-hash (group-by hash values)]
  (assert (= (len grouped-by-hash) (len values)))
  (for [v values] (assert (= (hash v) (hash v)))))
