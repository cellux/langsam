(import os)

(def test-path "/tmp/opentest.txt")
(def test-string "Hello, rumble\n")

(let [test-path "/tmp/opentest.txt"
      test-data "Hello, world!\n"]
  (let [f (os/open test-path (+ os/O_CREAT os/O_WRONLY) 0o666)]
    (os/write f test-string)
    (os/close f))
  (let [f (os/open test-path os/O_RDONLY)
        data (os/read f (len f))]
    (os/close f)
    (os/unlink test-path)
    (assert (= data test-string))))
