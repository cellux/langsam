; x86_64 assembler

(def register-infos
  {'al {:size 1 :code 0}
   'cl {:size 1 :code 1}
   'dl {:size 1 :code 2}
   'bl {:size 1 :code 3}
   'ah {:size 1 :code 4 :high8 true}
   'ch {:size 1 :code 5 :high8 true}
   'dh {:size 1 :code 6 :high8 true}
   'bh {:size 1 :code 7 :high8 true}
   'spl {:size 1 :code 4 :rex-required true}
   'bpl {:size 1 :code 5 :rex-required true}
   'sil {:size 1 :code 6 :rex-required true}
   'dil {:size 1 :code 7 :rex-required true}
   'r8b {:size 1 :code 8}
   'r9b {:size 1 :code 9}
   'r10b {:size 1 :code 10}
   'r11b {:size 1 :code 11}
   'r12b {:size 1 :code 12}
   'r13b {:size 1 :code 13}
   'r14b {:size 1 :code 14}
   'r15b {:size 1 :code 15}
   'ax {:size 2 :code 0}
   'cx {:size 2 :code 1}
   'dx {:size 2 :code 2}
   'bx {:size 2 :code 3}
   'sp {:size 2 :code 4}
   'bp {:size 2 :code 5}
   'si {:size 2 :code 6}
   'di {:size 2 :code 7}
   'r8w {:size 2 :code 8}
   'r9w {:size 2 :code 9}
   'r10w {:size 2 :code 10}
   'r11w {:size 2 :code 11}
   'r12w {:size 2 :code 12}
   'r13w {:size 2 :code 13}
   'r14w {:size 2 :code 14}
   'r15w {:size 2 :code 15}
   'eax {:size 4 :code 0}
   'ecx {:size 4 :code 1}
   'edx {:size 4 :code 2}
   'ebx {:size 4 :code 3}
   'esp {:size 4 :code 4}
   'ebp {:size 4 :code 5}
   'esi {:size 4 :code 6}
   'edi {:size 4 :code 7}
   'r8d {:size 4 :code 8}
   'r9d {:size 4 :code 9}
   'r10d {:size 4 :code 10}
   'r11d {:size 4 :code 11}
   'r12d {:size 4 :code 12}
   'r13d {:size 4 :code 13}
   'r14d {:size 4 :code 14}
   'r15d {:size 4 :code 15}
   'rax {:size 8 :code 0}
   'rcx {:size 8 :code 1}
   'rdx {:size 8 :code 2}
   'rbx {:size 8 :code 3}
   'rsp {:size 8 :code 4}
   'rbp {:size 8 :code 5}
   'rsi {:size 8 :code 6}
   'rdi {:size 8 :code 7}
   'r8 {:size 8 :code 8}
   'r9 {:size 8 :code 9}
   'r10 {:size 8 :code 10}
   'r11 {:size 8 :code 11}
   'r12 {:size 8 :code 12}
   'r13 {:size 8 :code 13}
   'r14 {:size 8 :code 14}
   'r15 {:size 8 :code 15}})

; -----------------------------------------------------------------------------
; Data-driven assembler tables (future encoder core)
;
; These tables are intentionally declarative and mostly instruction-agnostic.
; The idea is to move encode-instruction toward:
;   1) parse operands -> classify into operand classes
;   2) select matching form (mnemonic + operand classes + mode/features)
;   3) emit prefixes/map/opcode/modrm/sib/immediates from form templates
; -----------------------------------------------------------------------------

(def assembler-modes [:16 :32 :64])

(def legacy-prefix-by-keyword
  {:lock 0xF0
   :repne 0xF2
   :rep 0xF3
   :opsize 0x66
   :addrsize 0x67})

(def segment-prefix-by-register
  {'es 0x26
   'cs 0x2E
   'ss 0x36
   'ds 0x3E
   'fs 0x64
   'gs 0x65})

(def mandatory-prefix-by-tag
  {:none nil
   :66 0x66
   :f2 0xF2
   :f3 0xF3})

(def opcode-map-prefixes
  {:default []
   :0f [0x0F]
   :0f38 [0x0F 0x38]
   :0f3a [0x0F 0x3A]})

; Operand class DSL for instruction forms.
; :kind can be :reg, :rm, :imm, :rel, :moffs.
(def operand-class-specs
  {:r8 {:kind :reg :size 1}
   :r16 {:kind :reg :size 2}
   :r32 {:kind :reg :size 4}
   :r64 {:kind :reg :size 8}
   :rm8 {:kind :rm :size 1}
   :rm16 {:kind :rm :size 2}
   :rm32 {:kind :rm :size 4}
   :rm64 {:kind :rm :size 8}
   :m8 {:kind :rm :size 1 :mem-only true}
   :m16 {:kind :rm :size 2 :mem-only true}
   :m32 {:kind :rm :size 4 :mem-only true}
   :m64 {:kind :rm :size 8 :mem-only true}
   :imm8 {:kind :imm :size 1}
   :imm16 {:kind :imm :size 2}
   :imm32 {:kind :imm :size 4}
   :imm64 {:kind :imm :size 8}
   :rel8 {:kind :rel :size 1}
   :rel32 {:kind :rel :size 4}
   :moffs8 {:kind :moffs :size 1}
   :moffs16 {:kind :moffs :size 2}
   :moffs32 {:kind :moffs :size 4}
   :moffs64 {:kind :moffs :size 8}
   :xmm {:kind :reg :class :xmm}
   :ymm {:kind :reg :class :ymm}
   :zmm {:kind :reg :class :zmm}
   :k {:kind :reg :class :k}
   :st {:kind :reg :class :x87}})

; Encoding patterns describe where operands land (modrm/opcode+rd/imm).
(def encoding-patterns
  {:zo {:operands 0} ; zero operands
   :o {:operands 1 :opcode-plus-reg :op0} ; opcode + register code
   :oi {:operands 2 :opcode-plus-reg :op0 :imm :op1}
   :mi {:operands 2 :modrm {:reg :opcode-ext :rm :op0} :imm :op1}
   :mr {:operands 2 :modrm {:reg :op1 :rm :op0}}
   :rm {:operands 2 :modrm {:reg :op0 :rm :op1}}
   :m {:operands 1 :modrm {:reg :opcode-ext :rm :op0}}
   :i {:operands 1 :imm :op0}
   :d {:operands 1 :rel :op0}})

; Immediate encodings used by forms when sign-extension is special.
(def immediate-encoding-specs
  {:imm8 {:size 1}
   :imm16 {:size 2}
   :imm32 {:size 4}
   :imm32-sext64 {:size 4 :sign-extends-to 8}
   :imm64 {:size 8}
   :rel8 {:size 1 :relative true}
   :rel32 {:size 4 :relative true}})

; Useful for parsing aliases and condition mnemonics.
(def mnemonic-aliases
  {'sal 'shl
   'jz 'je
   'jnz 'jne
   'jc 'jb
   'jnc 'jae})

(def condition-code-nibbles
  {'o 0x0
   'no 0x1
   'b 0x2
   'c 0x2
   'nae 0x2
   'ae 0x3
   'nb 0x3
   'nc 0x3
   'e 0x4
   'z 0x4
   'ne 0x5
   'nz 0x5
   'be 0x6
   'na 0x6
   'a 0x7
   'nbe 0x7
   's 0x8
   'ns 0x9
   'p 0xA
   'pe 0xA
   'np 0xB
   'po 0xB
   'l 0xC
   'nge 0xC
   'ge 0xD
   'nl 0xD
   'le 0xE
   'ng 0xE
   'g 0xF
   'nle 0xF})

; Instruction forms: each mnemonic maps to many encoding forms.
; This table is intentionally partial right now, but schema is the important part.
(def instruction-form-table
  {'nop [{:operands []
          :encoding :zo
          :opcode-map :default
          :opcode 0x90}]
   'mov [{:operands [:r8 :rm8] :encoding :rm :opcode-map :default :opcode 0x8A}
         {:operands [:r16 :rm16] :encoding :rm :opcode-map :default :opcode 0x8B :opsize-prefix true}
         {:operands [:r32 :rm32] :encoding :rm :opcode-map :default :opcode 0x8B}
         {:operands [:r64 :rm64] :encoding :rm :opcode-map :default :opcode 0x8B :rexw true}
         {:operands [:rm8 :r8] :encoding :mr :opcode-map :default :opcode 0x88}
         {:operands [:rm16 :r16] :encoding :mr :opcode-map :default :opcode 0x89 :opsize-prefix true}
         {:operands [:rm32 :r32] :encoding :mr :opcode-map :default :opcode 0x89}
         {:operands [:rm64 :r64] :encoding :mr :opcode-map :default :opcode 0x89 :rexw true}
         {:operands [:rm8 :imm8] :encoding :mi :opcode-map :default :opcode 0xC6 :opcode-ext 0 :imm-kind :imm8}
         {:operands [:rm16 :imm16] :encoding :mi :opcode-map :default :opcode 0xC7 :opcode-ext 0 :opsize-prefix true :imm-kind :imm16}
         {:operands [:rm32 :imm32] :encoding :mi :opcode-map :default :opcode 0xC7 :opcode-ext 0 :imm-kind :imm32}
         {:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0xC7 :opcode-ext 0 :rexw true :imm-kind :imm32-sext64}]
   'add [{:operands [:rm8 :imm8] :encoding :mi :opcode-map :default :opcode 0x80 :opcode-ext 0 :imm-kind :imm8}
         {:operands [:rm16 :imm16] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 0 :opsize-prefix true :imm-kind :imm16}
         {:operands [:rm32 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 0 :imm-kind :imm32}
         {:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 0 :rexw true :imm-kind :imm32-sext64}]
   'or [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 1 :rexw true :imm-kind :imm32-sext64}]
   'adc [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 2 :rexw true :imm-kind :imm32-sext64}]
   'sbb [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 3 :rexw true :imm-kind :imm32-sext64}]
   'and [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 4 :rexw true :imm-kind :imm32-sext64}]
   'sub [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 5 :rexw true :imm-kind :imm32-sext64}]
   'xor [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 6 :rexw true :imm-kind :imm32-sext64}]
   'cmp [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext 7 :rexw true :imm-kind :imm32-sext64}]
   'test [{:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0xF7 :opcode-ext 0 :rexw true :imm-kind :imm32}]
   'rol [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 0 :rexw true :imm-kind :imm8}]
   'ror [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 1 :rexw true :imm-kind :imm8}]
   'rcl [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 2 :rexw true :imm-kind :imm8}]
   'rcr [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 3 :rexw true :imm-kind :imm8}]
   'shl [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 4 :rexw true :imm-kind :imm8}]
   'shr [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 5 :rexw true :imm-kind :imm8}]
   'sar [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext 7 :rexw true :imm-kind :imm8}]
   'bt [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext 4 :rexw true :imm-kind :imm8}]
   'bts [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext 5 :rexw true :imm-kind :imm8}]
   'btr [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext 6 :rexw true :imm-kind :imm8}]
   'btc [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext 7 :rexw true :imm-kind :imm8}]})

; Feature gates allow clean extension to AVX/AVX2/AVX-512, BMI, etc.
(def cpu-feature-tags
  [:base :x87 :sse :sse2 :sse3 :ssse3 :sse41 :sse42 :avx :avx2 :avx512
   :bmi1 :bmi2 :fma :aes :sha :gfni :amx])

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

(defn get-register-info [reg]
  (or (get register-infos reg)
      (throw (cons 'x86_64 (str "unsupported register: " reg)))))

(defn register-name? [v]
  (contains? register-infos v))

(defn reg-code-low3 [code]
  (bit-and code 0x7))

(defn reg-code-rex-bit [code]
  (bit-and (bit-shift-right code 3) 0x1))

(defn encode-rex [w r x b]
  (bit-or 0x40
          (if (nonzero? w) 0x08 0)
          (if (nonzero? r) 0x04 0)
          (if (nonzero? x) 0x02 0)
          (if (nonzero? b) 0x01 0)))

(defn encode-modrm [mod reg rm]
  (bit-or (bit-shift-left mod 6)
          (bit-shift-left reg 3)
          rm))

(defn encode-sib [scale index base]
  (bit-or (bit-shift-left scale 6)
          (bit-shift-left index 3)
          base))

(defn encode-imm32-le [imm]
  (if (or (< imm -0x80000000) (> imm 0x7fffffff))
    (throw (cons 'x86_64 (str "imm32 out of range: " imm))))
  (let [value (if (< imm 0) (+ 0x100000000 imm) imm)
        b0 (bit-and value 0xFF)
        x1 (bit-shift-right value 8)
        b1 (bit-and x1 0xFF)
        x2 (bit-shift-right x1 8)
        b2 (bit-and x2 0xFF)
        x3 (bit-shift-right x2 8)
        b3 (bit-and x3 0xFF)]
    [b0 b1 b2 b3]))

(defn encode-imm8 [imm]
  (if (or (< imm 0) (> imm 0xFF))
    (throw (cons 'x86_64 (str "imm8 out of range: " imm))))
  imm)

(defn encode-imm16-le [imm]
  (if (or (< imm -0x8000) (> imm 0xFFFF))
    (throw (cons 'x86_64 (str "imm16 out of range: " imm))))
  (let [value (if (< imm 0) (+ 0x10000 imm) imm)
        b0 (bit-and value 0xFF)
        x1 (bit-shift-right value 8)
        b1 (bit-and x1 0xFF)]
    [b0 b1]))

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

(defn encode-mem [memop]
  (let [{:keys [base index (scale 1) (disp 0) rip]} memop]
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
                                     (let [baseinfo (get-register-info base)
                                           base-code (if (= (:size baseinfo) 8)
                                                       (:code baseinfo)
                                                       (throw (cons 'x86_64
                                                                    (str "unsupported register: "
                                                                         base))))]
                                       [(reg-code-low3 base-code)
                                        (reg-code-rex-bit base-code)]))
              [index-low index-high] (if index
                                       (let [indexinfo (get-register-info index)
                                             index-code (if (= (:size indexinfo) 8)
                                                          (:code indexinfo)
                                                          (throw (cons 'x86_64
                                                                       (str "unsupported register: "
                                                                            index))))
                                             index-low (reg-code-low3 index-code)]
                                         (if (= index-low 4)
                                           (throw (cons 'x86_64
                                                        "rsp/r12 cannot be used as index")))
                                         [index-low (reg-code-rex-bit index-code)])
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

(def operand-ref-indexes
  {:op0 0
   :op1 1
   :op2 2
   :op3 3})

(defn imm-fits-size? [size imm]
  (case size
    1 (and (>= imm 0) (<= imm 0xFF))
    2 (and (>= imm -0x8000) (<= imm 0xFFFF))
    4 (and (>= imm -0x80000000) (<= imm 0x7fffffff))
    8 true
    false))

(defn encode-immediate-kind [imm-kind imm]
  (case imm-kind
    :imm8 [(encode-imm8 imm)]
    :imm16 (encode-imm16-le imm)
    :imm32 (encode-imm32-le imm)
    :imm32-sext64 (encode-imm32-le imm)
    :rel8 [(encode-simm8 imm)]
    :rel32 (encode-imm32-le imm)
    (throw (cons 'x86_64
                 (str "unsupported immediate encoding kind: " imm-kind)))))

(defn normalize-operand [operand]
  (if (and (symbol? operand) (register-name? operand))
    {:kind :reg :reg operand :reginfo (get-register-info operand)}
    (if (integer? operand)
      {:kind :imm :value operand}
      (let [memop (normalize-mem-operand operand)]
        (if (mem-operand? memop)
          {:kind :mem :memop memop}
          {:kind :other :value operand})))))

(defn match-operand-class [operand class-key]
  (let [spec (get operand-class-specs class-key)]
    (if (nil? spec)
      (throw (cons 'x86_64 (str "unknown operand class: " class-key))))
    (case (:kind spec)
      :reg
      (if (and (= (:kind operand) :reg)
               (if (:size spec)
                 (= (:size (:reginfo operand)) (:size spec))
                 true)
               (if (:class spec)
                 (= (:class (:reginfo operand)) (:class spec))
                 true))
        operand)
      :rm
      (if (or (= (:kind operand) :reg) (= (:kind operand) :mem))
        (let [size (:size spec)
              mem-only (:mem-only spec)]
          (if (and mem-only (= (:kind operand) :reg))
            nil
            (if (= (:kind operand) :reg)
              (if (or (nil? size) (= (:size (:reginfo operand)) size))
                operand)
              (let [memop (:memop operand)
                    mem-size (:size memop)]
                (if (and size mem-size (not= size mem-size))
                  nil
                  operand))))))
      :imm
      (if (and (= (:kind operand) :imm)
               (if (:size spec)
                 (imm-fits-size? (:size spec) (:value operand))
                 true))
        operand)
      :rel
      (if (and (= (:kind operand) :imm)
               (if (:size spec)
                 (imm-fits-size? (:size spec) (:value operand))
                 true))
        {:kind :rel :value (:value operand)})
      :moffs
      (if (= (:kind operand) :mem)
        operand)
      nil)))

(defn select-form-for-operands [forms normalized-operands]
  (let [it (iter forms)
        selected nil]
    (while (and it (nil? selected))
      (let [form @it
            expected (:operands form)]
        (if (= (len expected) (len normalized-operands))
          (let [it-expected (iter expected)
                it-operands (iter normalized-operands)
                ok true
                prepared nil]
            (while (and ok it-expected it-operands)
              (let [matched (match-operand-class @it-operands @it-expected)]
                (if matched
                  (setq prepared (cons matched prepared))
                  (setq ok false))
                (setq it-expected (next it-expected))
                (setq it-operands (next it-operands))))
            (if ok
              (setq selected {:form form :operands (Vector (nreverse prepared))}))))
        (setq it (next it))))
    selected))

(defn get-operand-by-ref [prepared-operands opref]
  (if-match [(Integer idx) (get operand-ref-indexes opref)]
    (get prepared-operands idx)
    (throw (cons 'x86_64 (str "invalid operand reference: " opref)))))

(defn encode-rex-with-flags [w r x b rex-required high8]
  (let [rex-needed (or (nonzero? w)
                       (nonzero? r)
                       (nonzero? x)
                       (nonzero? b)
                       rex-required)]
    (if (and high8 rex-needed)
      (throw (cons 'x86_64
                   "high 8-bit registers (ah/ch/dh/bh) cannot be encoded with REX")))
    (if rex-needed
      (encode-rex w r x b)
      nil)))

(defn collect-rex-register-flags [prepared-operands]
  (let [it (iter prepared-operands)
        rex-required false
        high8 false]
    (while it
      (let [operand @it]
        (if (= (:kind operand) :reg)
          (do
            (setq rex-required (or rex-required (:rex-required (:reginfo operand))))
            (setq high8 (or high8 (:high8 (:reginfo operand))))))
        (setq it (next it))))
    {:rex-required rex-required
     :high8 high8}))

(defn emit-form-encoding [form prepared-operands]
  (let [pattern (get encoding-patterns (:encoding form))]
    (if (nil? pattern)
      (throw (cons 'x86_64 (str "unknown encoding pattern: " (:encoding form)))))
    (let [opcode-base (:opcode form)
          opcode-map-prefix (or (get opcode-map-prefixes (:opcode-map form))
                                (get opcode-map-prefixes :default))
          mandatory-prefix (get mandatory-prefix-by-tag
                                (or (:mandatory-prefix form) :none))
          prefix-bytes (catv (if mandatory-prefix [mandatory-prefix] [])
                             (if (:opsize-prefix form) [0x66] []))
          rex-flags (collect-rex-register-flags prepared-operands)
          opcode-plus-reg-ref (:opcode-plus-reg pattern)
          opcode-plus-reg-op (if opcode-plus-reg-ref
                               (get-operand-by-ref prepared-operands opcode-plus-reg-ref))
          opcode (if opcode-plus-reg-ref
                   (+ opcode-base (reg-code-low3 (:code (:reginfo opcode-plus-reg-op))))
                   opcode-base)
          opcode-rexb (if opcode-plus-reg-ref
                        (reg-code-rex-bit (:code (:reginfo opcode-plus-reg-op)))
                        0)
          modrm-spec (:modrm pattern)
          reg-src (if modrm-spec (:reg modrm-spec))
          rm-src (if modrm-spec (:rm modrm-spec))
          reg-op (if (and modrm-spec (not= reg-src :opcode-ext))
                   (get-operand-by-ref prepared-operands reg-src))
          reg-low-bits (if modrm-spec
                         (if (= reg-src :opcode-ext)
                           (:opcode-ext form)
                           (reg-code-low3 (:code (:reginfo reg-op)))))
          rex-r (if modrm-spec
                  (if (= reg-src :opcode-ext)
                    0
                    (reg-code-rex-bit (:code (:reginfo reg-op))))
                  0)
          rm-op (if modrm-spec (get-operand-by-ref prepared-operands rm-src))
          rmenc (if modrm-spec
                  (if (= (:kind rm-op) :reg)
                    {:mod 3
                     :rm-low (reg-code-low3 (:code (:reginfo rm-op)))
                     :rexx 0
                     :rexb (reg-code-rex-bit (:code (:reginfo rm-op)))
                     :tail []}
                    (let [memenc (encode-mem (:memop rm-op))]
                      {:mod (:mod memenc)
                       :rm-low (:rm memenc)
                       :rexx (:rexx memenc)
                       :rexb (:rexb memenc)
                       :tail (:tail memenc)})))
          modrm-byte (if modrm-spec
                       (encode-modrm (:mod rmenc) reg-low-bits (:rm-low rmenc)))
          rex-w (if (:rexw form) 1 0)
          rex-x (if modrm-spec (:rexx rmenc) 0)
          rex-b (bit-or opcode-rexb (if modrm-spec (:rexb rmenc) 0))
          rex (encode-rex-with-flags rex-w
                                     rex-r
                                     rex-x
                                     rex-b
                                     (:rex-required rex-flags)
                                     (:high8 rex-flags))
          imm-ref (:imm pattern)
          imm-bytes (if imm-ref
                      (let [imm-op (get-operand-by-ref prepared-operands imm-ref)
                            imm-index (get operand-ref-indexes imm-ref)
                            default-imm-kind (get (:operands form) imm-index)
                            imm-kind (or (:imm-kind form) default-imm-kind)]
                        (encode-immediate-kind imm-kind (:value imm-op)))
                      [])]
      (catv prefix-bytes
            (if rex [rex] [])
            opcode-map-prefix
            [opcode]
            (if modrm-spec [modrm-byte] [])
            (if modrm-spec (:tail rmenc) [])
            imm-bytes))))

(defn encode-instruction-form-driven [instruction]
  (if-match [[(Symbol raw-mnemonic) & operands] instruction]
    (let [mnemonic (or (get mnemonic-aliases raw-mnemonic) raw-mnemonic)
          forms (get instruction-form-table mnemonic)]
      (if forms
        (let [normalized-operands (map normalize-operand operands)
              selected (select-form-for-operands forms normalized-operands)]
          (if selected
            (emit-form-encoding (:form selected)
                                (:operands selected))))))))

(defn encode-instruction [instruction]
  (or (encode-instruction-form-driven instruction)
      (throw (cons 'x86_64 (str "unsupported instruction: " instruction)))))
