(defclass StringStream
  [:source String {:required true}]
  [:offset Integer {:required true}])

(defn stream-read-end
  "Return the end index for read/peek where n<=0 means read to EOF."
  [(Integer start) (Integer source-len) (Integer n)]
  (if (<= n 0)
    source-len
    (min source-len (+ start n))))

(defmethod StringStream.eof
  "Return true when the stream offset is at or beyond source length."
  [self]
  (>= (:offset self) (len (:source self))))

(defmethod StringStream.read
  "Read n chars from the current offset; n<=0 reads to EOF."
  [self &opt ((Integer n) 0)]
  (let [source (:source self)
        start (:offset self)
        source-len (len source)
        end (stream-read-end start source-len n)
        part (if (= start end)
               ""
               (String (slice source start end)))]
    (put self :offset end)
    part))

(defmethod StringStream.peek
  "Return n chars without consuming them; n<=0 peeks to EOF."
  [self &opt ((Integer n) 0)]
  (let [source (:source self)
        start (:offset self)
        source-len (len source)
        end (stream-read-end start source-len n)]
    (if (= start end)
      ""
      (String (slice source start end)))))

(defmethod StringStream.read-char
  "Read and return one-character String, or nil at EOF."
  [self]
  (let [part (self.read 1)]
    (if (= (len part) 0)
      nil
      part)))

(defmethod StringStream.read-byte
  "Read one byte as Integer in 0..255, or nil at EOF."
  [self]
  (if (self.eof)
    nil
    (let [pos (:offset self)
          byte (get (:source self) pos)]
      (put self :offset (+ pos 1))
      byte)))

(defmethod StringStream.read-be
  "Read n bytes in big-endian order and return an Integer."
  [self (Integer n)]
  (if (<= n 0)
    (raise 'stream "StringStream.read-be expects positive Integer n, got %s" n))
  (let [remaining n
        result 0]
    (while (> remaining 0)
      (let [byte (self.read-byte)]
        (if (nil? byte)
          (raise 'stream "unexpected EOF in StringStream.read-be"))
        (setq result (bit-or (bit-shift-left result 8) byte)))
      (decq remaining))
    result))

(defmethod StringStream.read-le
  "Read n bytes in little-endian order and return an Integer."
  [self (Integer n)]
  (if (<= n 0)
    (raise 'stream "StringStream.read-le expects positive Integer n, got %s" n))
  (let [remaining n
        shift 0
        result 0]
    (while (> remaining 0)
      (let [byte (self.read-byte)]
        (if (nil? byte)
          (raise 'stream "unexpected EOF in StringStream.read-le"))
        (setq result (bit-or result (bit-shift-left byte shift))))
      (setq shift (+ shift 8))
      (decq remaining))
    result))

(let [string-match-at? (fn string-match-at?
                         "Return true when marker bytes match source at index at."
                         [(String source) (Integer at) (String marker) (Integer marker-len)]
                         (let [j 0
                               ok true]
                           (while (and ok (< j marker-len))
                             (if (= (get source (+ at j)) (get marker j))
                               (incq j)
                               (setq ok false)))
                           ok))
      string-find-from (fn string-find-from
                         "Return first marker index in source at/after start, else nil."
                         [(String source) (Integer start) (String marker)]
                         (let [source-len (len source)
                               marker-len (len marker)]
                           (if (= marker-len 0)
                             (raise 'stream "read-until marker cannot be empty"))
                           (let [head (get marker 0)
                                 i start
                                 limit (- source-len marker-len)
                                 found nil]
                             (while (and (<= i limit) (nil? found))
                               (if (and (= (get source i) head)
                                        (string-match-at? source i marker marker-len))
                                 (setq found i)
                                 (incq i)))
                             found)))]
  (defmethod StringStream.read-until
    "Read until marker. Returns [part found?] unless keep-marker=true."
    [self (String marker) &opt (keep-marker false)]
    (let [source (:source self)
          start (:offset self)
          source-len (len source)
          marker-len (len marker)
          marker-pos (string-find-from source start marker)]
      (if marker-pos
        (let [part-end marker-pos
              stream-end (+ marker-pos marker-len)
              part (if (= start part-end)
                     ""
                     (String (slice source start part-end)))
              part+marker (if (= start stream-end)
                            ""
                            (String (slice source start stream-end)))]
          (put self :offset stream-end)
          (if keep-marker
            part+marker
            [part true]))
        (let [part (if (= start source-len)
                     ""
                     (String (slice source start source-len)))]
          (put self :offset source-len)
          (if keep-marker
            part
            [part false]))))))

(defmethod StringStream.readln
  "Read a single line terminated by \\n without including the terminator."
  [self]
  (let [[line _] (self.read-until "\n")]
    line))
