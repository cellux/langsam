(defclass FileStream
  {:extends BaseStream}
  [:file File {:required true}]
  [:buffer String {:required true}]
  [:ended Boolean {:required true}])

(defn -filestream-fill
  "Fill buffer until it has at least want chars, or EOF when want<=0."
  [self (Integer want)]
  (while (and (not (:ended self))
              (or (<= want 0)
                  (< (len (:buffer self)) want)))
    (let [part (read (:file self) 1)]
      (if (nil? part)
        (put self :ended true)
        (put self :buffer (str (:buffer self) part))))))

(defn -filestream-take
  "Take n chars from buffer and consume them."
  [self (Integer n)]
  (let [buf (:buffer self)
        buf-len (len buf)
        end (min buf-len n)
        part (if (= end 0)
               ""
               (String (slice buf 0 end)))
        rest (if (= end buf-len)
               ""
               (String (slice buf end buf-len)))]
    (put self :buffer rest)
    part))

(defmethod FileStream.eof
  "Return true when no buffered chars remain and file is exhausted."
  [self]
  (if (> (len (:buffer self)) 0)
    false
    (do
      (-filestream-fill self 1)
      (and (:ended self)
           (= (len (:buffer self)) 0)))))

(defmethod FileStream.read
  "Read n chars from the stream; n<=0 reads to EOF."
  [self &opt ((Integer n) 0)]
  (if (<= n 0)
    (do
      (-filestream-fill self 0)
      (-filestream-take self (len (:buffer self))))
    (do
      (-filestream-fill self n)
      (-filestream-take self n))))

(defmethod FileStream.peek
  "Return n chars without consuming them; n<=0 peeks to EOF."
  [self &opt ((Integer n) 0)]
  (if (<= n 0)
    (do
      (-filestream-fill self 0)
      (:buffer self))
    (do
      (-filestream-fill self n)
      (let [buf (:buffer self)
            end (min (len buf) n)]
        (if (= end 0)
          ""
          (String (slice buf 0 end)))))))

(defmethod FileStream.read-until
  "Read until marker. Returns [part found?] unless keep-marker=true."
  [self (String marker) &opt (keep-marker false)]
  (let [marker-len (len marker)]
    (if (= marker-len 0)
      (raise 'stream "read-until marker cannot be empty"))
    (let [part+marker ""
          found false
          reading true]
      (while reading
        (let [ch (self.read-char)]
          (if (nil? ch)
            (setq reading false)
            (do
              (setq part+marker (str part+marker ch))
              (let [part-len (len part+marker)]
                (if (and (>= part-len marker-len)
                         (= marker
                            (String (slice part+marker
                                           (- part-len marker-len)
                                           part-len))))
                  (do
                    (setq found true)
                    (setq reading false))))))))
      (if keep-marker
        part+marker
        (if found
          (let [part-len (len part+marker)
                split-at (- part-len marker-len)]
            [(if (= split-at 0)
               ""
               (String (slice part+marker 0 split-at)))
             true])
          [part+marker false])))))

(defmethod FileStream.close
  "Close the underlying file descriptor."
  [self]
  (let [result (close (:file self))]
    (if (nil? result)
      (do
        (put self :buffer "")
        (put self :ended true)))
    result))

(defmethod make-stream File
  "Create a FileStream from an os/File."
  [file]
  (FileStream {:file file :buffer "" :ended false}))

(defmethod make-stream FileStream
  "Return file streams unchanged."
  [s]
  s)
