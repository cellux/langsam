(let [s (stream "")]
  (assert (s.eof)))

(let [s (stream "hello")]
  (assert (not (s.eof)))
  (assert (= (s.read) "hello"))
  (assert (s.eof)))

(let [s (stream "hello\n")]
  (assert (= (s.readln) "hello"))
  (assert (s.eof)))

(let [s (stream "hello")]
  (assert (= (s.read 2) "he"))
  (assert (= (s.read 3) "llo"))
  (assert (s.eof)))

(let [s (stream "hello")]
  (assert (= (s.read 0) "hello")) ; read until eof
  (assert (s.eof)))

(let [s (stream "hello")]
  (assert (= (s.read-char) "h"))
  (assert (= (s.read-byte) 0x65)))

(let [s (stream "\x01\x02\x03\x04\x05\x06\x07")]
  (assert (= (s.read-be 1) 0x01))
  (assert (= (s.read-be 2) 0x0203))
  (assert (= (s.read-be 4) 0x04050607)))

(let [s (stream "\x01\x02\x03\x04\x05\x06\x07")]
  (assert (= (s.read-le 1) 0x01))
  (assert (= (s.read-le 2) 0x0302))
  (assert (= (s.read-le 4) 0x07060504)))

(let [s (stream "hello")]
  (assert (= (s.peek 2) "he"))
  (assert (= (s.read) "hello")))

(let [s (stream "\"This is not too good\", he said.")]
  (let [[part found-marker] (s.read-until "\"")]
    (assert (= part ""))
    (assert (= found-marker true))
    (assert (not (s.eof))))
  (let [[part found-marker] (s.read-until "\"")]
    (assert (= part "This is not too good"))
    (assert (= found-marker true))
    (assert (not (s.eof))))
  (let [[part found-marker] (s.read-until "\"")]
    (assert (= part ", he said."))
    (assert (= found-marker false))
    (assert (s.eof)))
  (let [[part found-marker] (s.read-until "\"")]
    (assert (= part ""))
    (assert (= found-marker false))
    (assert (s.eof))))

(let [s (stream "\"This is not too good\", he said.")]
  (assert (= (s.read-until "," true)) "\"This is not too good\",") ; true: keep marker
  (assert (= (s.read-until "," true)) " he said."))
