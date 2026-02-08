(import x86_64)

(defn bench-map [n]
  (let [m {}
        i 0
        acc 0]
    (while (< i n)
      (put m i (+ i 1))
      (setq i (+ i 1)))
    (setq i 0)
    (while (< i n)
      (setq acc (+ acc (get m i)))
      (setq i (+ i 1)))
    acc))

(defn bench-call [n]
  (let [f (fn [x] (+ x 1))
        g (fn [x] (f (f (f x))))
        i 0
        acc 0]
    (while (< i n)
      (setq acc (+ acc (g i)))
      (setq i (+ i 1)))
    acc))

(defn bench-x86 [n]
  (let [i 0
        acc 0]
    (while (< i n)
      (let [bytes (x86_64/encode-instruction '(mov rax (mem :size 8 :base rbx :index rcx :scale 4 :disp 0x1234)))]
        (setq acc (+ acc (len bytes))))
      (setq i (+ i 1)))
    acc))

(assert (= (bench-map 1000) 500500))
(assert (= (bench-call 5000) 12512500))
(assert (= (bench-x86 400) 3200))
