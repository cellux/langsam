(defclass Stream)

(defmethod Stream.read-char
  "Read and return one-character String, or nil at EOF."
  [self]
  (let [part (self.read 1)]
    (if (= (len part) 0)
      nil
      part)))

(defmethod Stream.read-byte
  "Read one byte as Integer in 0..255, or nil at EOF."
  [self]
  (let [part (self.read-char)]
    (if (nil? part)
      nil
      (get part 0))))

(defmethod Stream.read-be
  "Read n bytes in big-endian order and return an Integer."
  [self (Integer n)]
  (let [class-name (%name (class self))]
    (if (<= n 0)
      (raise 'stream "%s.read-be expects positive Integer n, got %s" class-name n))
    (let [remaining n
          result 0]
      (while (> remaining 0)
        (let [byte (self.read-byte)]
          (if (nil? byte)
            (raise 'stream "unexpected EOF in %s.read-be" class-name))
          (setq result (bit-or (bit-shift-left result 8) byte)))
        (decq remaining))
      result)))

(defmethod Stream.read-le
  "Read n bytes in little-endian order and return an Integer."
  [self (Integer n)]
  (let [class-name (%name (class self))]
    (if (<= n 0)
      (raise 'stream "%s.read-le expects positive Integer n, got %s" class-name n))
    (let [remaining n
          shift 0
          result 0]
      (while (> remaining 0)
        (let [byte (self.read-byte)]
          (if (nil? byte)
            (raise 'stream "unexpected EOF in %s.read-le" class-name))
          (setq result (bit-or result (bit-shift-left byte shift))))
        (setq shift (+ shift 8))
        (decq remaining))
      result)))

(defmethod Stream.readln
  "Read a single line terminated by \\n without including the terminator."
  [self]
  (let [[line _] (self.read-until "\n")]
    line))
