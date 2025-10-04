(import os)

(let [f (os/open "/tmp/opentest.txt" (+ os/O_CREAT os/O_WRONLY) 0o666)]
  (os/write f "Hello, world!\n")
  (os/close f))
