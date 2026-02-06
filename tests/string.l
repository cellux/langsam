(assert (= (type "foo") String))
(assert (= (String "foo") "foo"))
(assert (= (str "foo") "foo"))
(assert (= (str "foo" "bar") "foobar"))
(assert (= (str "foo" 523 "bar") "foo523bar"))

(assert (= (str 'alpha) "alpha"))
(assert (= (str 'alpha 'bravo) "alphabravo"))
(assert (= (apply str 'alpha 'bravo '(charlie delta)) "alphabravocharliedelta"))

(assert (= (repr "") "\"\""))
(assert (= (repr "foo") "\"foo\""))
(assert (= (repr "foo\"bar") "\"foo\\\"bar\""))
(assert (= (repr "foo\\bar") "\"foo\\\\bar\""))
(assert (= (repr "foo\a\b\f\n\r\t\vbar")
           "\"foo\\a\\b\\f\\n\\r\\t\\vbar\""))

(assert (< "alpha" "bravo"))
(assert (< "alpha" "charlie"))
(assert (< "charlie" "delta"))
(assert (> "delta" "charlie"))
(assert (> "deltaforce" "delta"))

(assert (= (+ "foo" "bar") "foobar"))

(assert (= (get "foobar" 3) 98))
(assert (= (get "foobar" -1) 114))
(assert (= (get "foobar" -2) 97))

(assert (= (len "foobar") 6))
