(assert (= '@foo '(deref foo)))
(assert (= 'os/open `(,get os (,quote open))))
(assert (= 'f.write `(,cons f (,quote write))))
