(assert (class? Stream))
(assert (= (stream "") {:source "" :offset 0}))

(assert (class? StringStream))
(let [s (StringStream {:source "ab" :offset 0})]
  (assert (= (s.read-char) "a"))
  (assert (= (s.read-char) "b"))
  (assert (= (s.read-char) nil)))

(import os :refer [O_RDONLY open])

; import with :refer does not bind the module symbol
(assert (nil? os))

(assert (integer? O_RDONLY))
(assert (function? open))

(import os)
(assert (map? os))
(assert (integer? os/O_RDONLY))
(assert (function? os/open))
