(let [e (error "foo")]
  (assert (= (type e) Error))
  (assert (= (type (Error e)) Error))
  (assert (= @e "foo")))

(let [e (error "this failed %d times" 5)]
  (assert (= @e "this failed 5 times"))
  (assert (= (str e) "this failed 5 times"))
  (assert (= (repr e) "(error '\"this failed 5 times\")")))

(let [e (error [:special :error])]
  (assert (= (type e) Error))
  (assert (= @e [:special :error]))
  (assert (= (str e) "[:special :error]"))
  (assert (= (repr e) "(error '[:special :error])")))

(let [e (error '(:not-found . "this is a 404 error"))]
  (assert (= @e '(:not-found . "this is a 404 error")))
  (assert (= (str e) "this is a 404 error"))
  (assert (= (repr e) "(error '(:not-found . \"this is a 404 error\"))")))
