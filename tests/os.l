(import os)

(assert (= (type os) Map))
(assert (= (type os/open) Function))

(def test-path "/tmp/opentest.txt")
(def test-string "Hello, rumble\n")

(let [test-path "/tmp/opentest.txt"
      test-data "Hello, world!\n"]
  (try
    (let [f (os/open test-path (+ os/O_CREAT os/O_WRONLY) 0o666)]
      (os/write f test-string)
      (os/close f))
    (let [f (os/open test-path os/O_RDONLY)
          data (os/read f (len f))]
      (os/close f)
      (assert (= data test-string)))
    (%finally (catch [_ nil] (os/unlink test-path)))))

(let [test-path "/tmp/filestream.txt"
      test-data "hello, rumble\nworld"]
  (try
    (let [f (os/open test-path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
      (os/write f test-data)
      (os/close f))
    (let [f (os/open test-path os/O_RDONLY)
          s (stream f)]
      (assert (not (s.eof)))
      (assert (= (s.peek 2) "he"))
      (assert (= (s.read 5) "hello"))
      (assert (= (s.readln) ", rumble"))
      (assert (= (s.read-until "r") ["wo" true]))
      (assert (= (s.read) "ld"))
      (assert (s.eof))
      (assert (= (s.read-char) nil))
      (s.close))
    (%finally (catch [_ nil] (os/unlink test-path)))))

(let [test-path "/tmp/filestream-bytes.bin"
      test-data "\x01\x02\x03\x04\x05\x06\x07"]
  (try
    (let [f (os/open test-path (+ os/O_CREAT os/O_WRONLY os/O_TRUNC) 0o666)]
      (os/write f test-data)
      (os/close f))
    (let [s (stream (os/open test-path os/O_RDONLY))]
      (assert (= (s.read-be 1) 0x01))
      (assert (= (s.read-be 2) 0x0203))
      (assert (= (s.read-le 2) 0x0504))
      (assert (= (s.read-byte) 0x06))
      (assert (= (s.read-byte) 0x07))
      (assert (= (s.read-byte) nil))
      (s.close))
    (%finally (catch [_ nil] (os/unlink test-path)))))
