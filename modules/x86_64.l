; x86_64 assembler

(def r64-register-codes
  {'rax 0
   'rcx 1
   'rdx 2
   'rbx 3
   'rsp 4
   'rbp 5
   'rsi 6
   'rdi 7
   'r8 8
   'r9 9
   'r10 10
   'r11 11
   'r12 12
   'r13 13
   'r14 14
   'r15 15})

(def group1-r64-imm32-opcode-ext
  {'add 0
   'or 1
   'adc 2
   'sbb 3
   'and 4
   'sub 5
   'xor 6
   'cmp 7})

(def shift-r64-imm8-opcode-ext
  {'rol 0
   'ror 1
   'rcl 2
   'rcr 3
   'shl 4
   'sal 4
   'shr 5
   'sar 7})

(def bt-r64-imm8-opcode-ext
  {'bt 4
   'bts 5
   'btr 6
   'btc 7})

(def scale-encodings
  {1 0
   2 1
   4 2
   8 3})

(defn get-r64-code [reg]
  (or (get r64-register-codes reg)
      (throw (cons 'x86_64 (str "unsupported register: " reg)))))

(defn register-name? [v]
  (contains? r64-register-codes v))

(defn reg-low [code]
  (mod code 8))

(defn reg-high [code]
  (if (>= code 8) 1 0))

(defn encode-rex-w [r x b]
  (+ 0x48
     (if (nonzero? r) 0x04 0)
     (if (nonzero? x) 0x02 0)
     (if (nonzero? b) 0x01 0)))

(defn encode-modrm [mod reg rm]
  (+ (bit-shift-left mod 6)
     (bit-shift-left reg 3)
     rm))

(defn encode-sib [scale index base]
  (+ (bit-shift-left scale 6)
     (bit-shift-left index 3)
     base))

(defn encode-imm32-le [imm]
  (if (or (< imm -0x80000000) (> imm 0x7fffffff))
    (throw (cons 'x86_64 (str "imm32 out of range: " imm))))
  (let [value (if (< imm 0) (+ 0x100000000 imm) imm)
        b0 (mod value 0x100)
        x1 (bit-shift-right value 8)
        b1 (mod x1 0x100)
        x2 (bit-shift-right x1 8)
        b2 (mod x2 0x100)
        x3 (bit-shift-right x2 8)
        b3 (mod x3 0x100)]
    [b0 b1 b2 b3]))

(defn encode-imm8 [imm]
  (if (or (< imm 0) (> imm 0xFF))
    (throw (cons 'x86_64 (str "imm8 out of range: " imm))))
  imm)

(defn encode-simm8 [imm]
  (if (or (< imm -128) (> imm 127))
    (throw (cons 'x86_64 (str "simm8 out of range: " imm))))
  (if (< imm 0) (+ 0x100 imm) imm))

(defn mem-from-opts [opts forced-size]
  (let [m {:kind :mem}
        it (iter opts)]
    (if forced-size
      (put m :size forced-size))
    (while it
      (let [k @it]
        (setq it (next it))
        (if (not it)
          (throw (cons 'x86_64 "mem expects key/value pairs")))
        (let [v @it]
          (if (and forced-size (= k :size))
            (throw (cons 'x86_64 "size is implicit in memq/memd/memw/memb")))
          (put m k v)
          (setq it (next it)))))
    m))

(defn mem [& rest]
  (mem-from-opts rest nil))

(defn memq [& rest]
  (mem-from-opts rest 8))

(defn memd [& rest]
  (mem-from-opts rest 4))

(defn memw [& rest]
  (mem-from-opts rest 2))

(defn memb [& rest]
  (mem-from-opts rest 1))

(defn mem-operand? [operand]
  (and (map? operand)
       (= (:kind operand) :mem)))

(defn normalize-mem-operand [operand]
  (case operand
    (Map m) (if (= (:kind m) :mem)
              m
              (throw (cons 'x86_64 (str "not a mem operand:" operand))))
    `(mem ,@opts) (mem-from-opts opts nil)
    `(memq ,@opts) (mem-from-opts opts 8)
    `(memd ,@opts) (mem-from-opts opts 4)
    `(memw ,@opts) (mem-from-opts opts 2)
    `(memb ,@opts) (mem-from-opts opts 1)
    operand))

(defn encode-mem64 [memop]
  (let [{:keys [size base index (scale 1) (disp 0) rip]} memop]
    (if (not= size 8)
      (throw (cons 'x86_64 (str "unsupported mem size in mov: " size))))
    (if (and base (not (symbol? base)))
      (throw (cons 'x86_64 (str "base should be a Symbol, got: " base))))
    (if (and index (not (symbol? index)))
      (throw (cons 'x86_64 (str "index should be a Symbol, got: " index))))
    (if (not (integer? disp))
      (throw (cons 'x86_64 (str "disp should be an Integer, got: " disp))))
    (if (and rip (not (integer? rip)))
      (throw (cons 'x86_64
                   (str "rip should be an Integer displacement, got: "
                        rip))))
    (if (and (nil? index) (not= scale 1))
      (throw (cons 'x86_64 "scale requires an index register")))

    (if-let [scale-bits (get scale-encodings scale)]
      (if rip
        (do
          (if (or base index)
            (throw (cons 'x86_64
                         "rip-relative addressing cannot include :base or :index")))
          {:mod 0
           :rm 5
           :rexx 0
           :rexb 0
           :tail (encode-imm32-le rip)})
        (let [no-base? (nil? base)
              [base-low base-high] (if no-base?
                                     [5 0]
                                     (let [base-code (get-r64-code base)]
                                       [(reg-low base-code)
                                        (reg-high base-code)]))
              [index-low index-high] (if index
                                       (let [index-code (get-r64-code index)
                                             index-low (reg-low index-code)]
                                         (if (= index-low 4)
                                           (throw (cons 'x86_64
                                                        "rsp/r12 cannot be used as index")))
                                         [index-low (reg-high index-code)])
                                       [4 0])
              needs-sib (or no-base? index (= base-low 4))
              [mod disp-bytes] (if no-base?
                                 [0 (encode-imm32-le disp)]
                                 (if (and (= disp 0) (not= base-low 5))
                                   [0 []]
                                   (if (and (>= disp -128) (<= disp 127))
                                     [1 [(encode-simm8 disp)]]
                                     [2 (encode-imm32-le disp)])))
              rm (if needs-sib 4 base-low)
              sib-bytes (if needs-sib
                          [(encode-sib scale-bits index-low base-low)]
                          [])]
          {:mod mod
           :rm rm
           :rexx index-high
           :rexb base-high
           :tail (catv sib-bytes disp-bytes)}))
      (throw (cons 'x86_64 (str "unsupported scale: " scale))))))

(defn encode-mov-r64-r64 [dst src]
  (let [dst-code (get-r64-code dst)
        src-code (get-r64-code src)
        rex (encode-rex-w (reg-high dst-code)
                          0
                          (reg-high src-code))
        modrm (encode-modrm 3 (reg-low dst-code) (reg-low src-code))]
    [rex 0x8B modrm]))

(defn encode-mov-r64-mem [dst memop]
  (let [dst-code (get-r64-code dst)
        memenc (encode-mem64 memop)
        rex (encode-rex-w (reg-high dst-code)
                          (:rexx memenc)
                          (:rexb memenc))
        modrm (encode-modrm (:mod memenc)
                            (reg-low dst-code)
                            (:rm memenc))]
    (catv [rex 0x8B modrm] (:tail memenc))))

(defn encode-mov-mem-r64 [memop src]
  (let [src-code (get-r64-code src)
        memenc (encode-mem64 memop)
        rex (encode-rex-w (reg-high src-code)
                          (:rexx memenc)
                          (:rexb memenc))
        modrm (encode-modrm (:mod memenc)
                            (reg-low src-code)
                            (:rm memenc))]
    (catv [rex 0x89 modrm] (:tail memenc))))

(defn encode-instruction [instruction]
  (case instruction
    `(nop) [0x90]
    `(mov ,reg ,(Integer imm)) (let [code (get-r64-code reg)
                                     rex (encode-rex-w 0 0 (reg-high code))
                                     modrm (encode-modrm 3 0 (reg-low code))
                                     [b0 b1 b2 b3] (encode-imm32-le imm)]
                                 [rex 0xC7 modrm b0 b1 b2 b3])
    `(mov ,dst ,src) (let [dst (normalize-mem-operand dst)
                           src (normalize-mem-operand src)]
                       (cond-let
                         [ok (and (register-name? dst)
                                  (register-name? src))]
                         (encode-mov-r64-r64 dst src)
                         [ok (and (register-name? dst)
                                  (mem-operand? src))]
                         (encode-mov-r64-mem dst src)
                         [ok (and (mem-operand? dst)
                                  (register-name? src))]
                         (encode-mov-mem-r64 dst src)
                         (throw (cons 'x86_64
                                      (str "unsupported instruction: "
                                           instruction)))))
    `(test ,reg ,(Integer imm)) (let [code (get-r64-code reg)
                                      rex (encode-rex-w 0 0 (reg-high code))
                                      modrm (encode-modrm 3 0 (reg-low code))
                                      [b0 b1 b2 b3] (encode-imm32-le imm)]
                                  [rex 0xF7 modrm b0 b1 b2 b3])
    `(,inst ,reg ,(Integer imm)) (cond-let
                                   [group1-ext
                                    (get group1-r64-imm32-opcode-ext inst)]
                                   (let [code (get-r64-code reg)
                                         rex (encode-rex-w 0 0 (reg-high code))
                                         modrm (encode-modrm 3
                                                             group1-ext
                                                             (reg-low code))
                                         [b0 b1 b2 b3] (encode-imm32-le imm)]
                                     [rex 0x81 modrm b0 b1 b2 b3])
                                   [shift-ext
                                    (get shift-r64-imm8-opcode-ext inst)]
                                   (let [code (get-r64-code reg)
                                         rex (encode-rex-w 0 0 (reg-high code))
                                         modrm (encode-modrm 3
                                                             shift-ext
                                                             (reg-low code))
                                         imm8 (encode-imm8 imm)]
                                     [rex 0xC1 modrm imm8])
                                   [bt-ext
                                    (get bt-r64-imm8-opcode-ext inst)]
                                   (let [code (get-r64-code reg)
                                         rex (encode-rex-w 0 0 (reg-high code))
                                         modrm (encode-modrm 3
                                                             bt-ext
                                                             (reg-low code))
                                         imm8 (encode-imm8 imm)]
                                     [rex 0x0F 0xBA modrm imm8])
                                   (throw (cons 'x86_64
                                                (str "unsupported instruction: "
                                                     instruction))))
    (throw (cons 'x86_64 (str "unsupported instruction: " instruction)))))
