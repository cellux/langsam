(import stream :refer [Stream])

(assert (class? Stream))
(assert (= (stream "") {:source "" :offset 0}))

(import string :refer [StringStream])

(assert (class? StringStream))
(let [s (StringStream {:source "ab" :offset 0})]
  (assert (= (s.read-char) "a"))
  (assert (= (s.read-char) "b"))
  (assert (= (s.read-char) nil)))
