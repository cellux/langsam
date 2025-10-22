(assert (= (type "foo") String))
(assert (= (String "foo") "foo"))
(assert (= (str "foo") "foo"))
(assert (= (str "foo" "bar") "foobar"))
(assert (= (str "foo" 523 "bar") "foo523bar"))

(assert (= (str 'alpha) "alpha"))
(assert (= (str 'alpha 'bravo) "alphabravo"))
(let [dotted-args '(charlie delta)]
  (assert (= (str 'alpha 'bravo . dotted-args) "alphabravocharliedelta")))
