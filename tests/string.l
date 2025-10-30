(assert (= (type "foo") String))
(assert (= (String "foo") "foo"))
(assert (= (str "foo") "foo"))
(assert (= (str "foo" "bar") "foobar"))
(assert (= (str "foo" 523 "bar") "foo523bar"))

(assert (= (str 'alpha) "alpha"))
(assert (= (str 'alpha 'bravo) "alphabravo"))
(let [rest '(charlie delta)]
  (assert (= (apply str 'alpha 'bravo rest) "alphabravocharliedelta")))
