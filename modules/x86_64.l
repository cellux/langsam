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
   :simm8 {:kind :imm :size 1 :signed true}
   :imm16 {:kind :imm :size 2}
   :imm32 {:kind :imm :size 4}
   :imm64 {:kind :imm :size 8}
   :rel8 {:kind :rel :size 1 :signed true}
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
   :rmi {:operands 3 :modrm {:reg :op0 :rm :op1} :imm :op2}
   :mi {:operands 2 :modrm {:reg :opcode-ext :rm :op0} :imm :op1}
   :mr {:operands 2 :modrm {:reg :op1 :rm :op0}}
   :rm {:operands 2 :modrm {:reg :op0 :rm :op1}}
   :m {:operands 1 :modrm {:reg :opcode-ext :rm :op0}}
   :i {:operands 1 :imm :op0}
   :d {:operands 1 :rel :op0}})

; Useful for parsing aliases and condition mnemonics.
(def mnemonic-aliases
  {'sal 'shl
   'jz 'je
   'jnz 'jne
   'jc 'jb
   'jnc 'jae})

; Dynamic condition-mnemonic maps.
(def jcc-mnemonic-nibbles
  {'jo 0x0
   'jno 0x1
   'jb 0x2
   'jc 0x2
   'jnae 0x2
   'jae 0x3
   'jnb 0x3
   'jnc 0x3
   'je 0x4
   'jz 0x4
   'jne 0x5
   'jnz 0x5
   'jbe 0x6
   'jna 0x6
   'ja 0x7
   'jnbe 0x7
   'js 0x8
   'jns 0x9
   'jp 0xA
   'jpe 0xA
   'jnp 0xB
   'jpo 0xB
   'jl 0xC
   'jnge 0xC
   'jge 0xD
   'jnl 0xD
   'jle 0xE
   'jng 0xE
   'jg 0xF
   'jnle 0xF})

(def setcc-mnemonic-nibbles
  {'seto 0x0
   'setno 0x1
   'setb 0x2
   'setc 0x2
   'setnae 0x2
   'setae 0x3
   'setnb 0x3
   'setnc 0x3
   'sete 0x4
   'setz 0x4
   'setne 0x5
   'setnz 0x5
   'setbe 0x6
   'setna 0x6
   'seta 0x7
   'setnbe 0x7
   'sets 0x8
   'setns 0x9
   'setp 0xA
   'setpe 0xA
   'setnp 0xB
   'setpo 0xB
   'setl 0xC
   'setnge 0xC
   'setge 0xD
   'setnl 0xD
   'setle 0xE
   'setng 0xE
   'setg 0xF
   'setnle 0xF})

(defn build-group1-forms
  "Build arithmetic/logical group-1 instruction forms for an opcode extension."
  [opcode-ext]
  (let [opcode8 (+ 0x00 (* opcode-ext 8))
        opcodew (+ 0x01 (* opcode-ext 8))
        opcode8r (+ 0x02 (* opcode-ext 8))
        opcodewr (+ 0x03 (* opcode-ext 8))]
    [{:operands [:rm8 :r8] :encoding :mr :opcode-map :default :opcode opcode8}
     {:operands [:rm16 :r16] :encoding :mr :opcode-map :default :opcode opcodew :opsize-prefix true}
     {:operands [:rm32 :r32] :encoding :mr :opcode-map :default :opcode opcodew}
     {:operands [:rm64 :r64] :encoding :mr :opcode-map :default :opcode opcodew :rexw true}
     {:operands [:r8 :rm8] :encoding :rm :opcode-map :default :opcode opcode8r}
     {:operands [:r16 :rm16] :encoding :rm :opcode-map :default :opcode opcodewr :opsize-prefix true}
     {:operands [:r32 :rm32] :encoding :rm :opcode-map :default :opcode opcodewr}
     {:operands [:r64 :rm64] :encoding :rm :opcode-map :default :opcode opcodewr :rexw true}
     {:operands [:rm8 :imm8] :encoding :mi :opcode-map :default :opcode 0x80 :opcode-ext opcode-ext :imm-kind :imm8}
     {:operands [:rm16 :imm16] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext opcode-ext :opsize-prefix true :imm-kind :imm16}
     {:operands [:rm32 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext opcode-ext :imm-kind :imm32}
     {:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0x81 :opcode-ext opcode-ext :rexw true :imm-kind :imm32-sext64}
     {:operands [:rm16 :simm8] :encoding :mi :opcode-map :default :opcode 0x83 :opcode-ext opcode-ext :opsize-prefix true :imm-kind :simm8}
     {:operands [:rm32 :simm8] :encoding :mi :opcode-map :default :opcode 0x83 :opcode-ext opcode-ext :imm-kind :simm8}
     {:operands [:rm64 :simm8] :encoding :mi :opcode-map :default :opcode 0x83 :opcode-ext opcode-ext :rexw true :imm-kind :simm8}]))

(defn build-shift-forms
  "Build immediate-count shift/rotate forms for a group opcode extension."
  [opcode-ext]
  [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext opcode-ext :rexw true :imm-kind :imm8}
   {:operands [:rm32 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext opcode-ext :imm-kind :imm8}
   {:operands [:rm16 :imm8] :encoding :mi :opcode-map :default :opcode 0xC1 :opcode-ext opcode-ext :opsize-prefix true :imm-kind :imm8}])

(defn build-bt-forms
  "Build bt/bts/btr/btc immediate-bit-test forms for 32/64-bit operands."
  [opcode-ext]
  [{:operands [:rm64 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext opcode-ext :rexw true :imm-kind :imm8}
   {:operands [:rm32 :imm8] :encoding :mi :opcode-map :0f :opcode 0xBA :opcode-ext opcode-ext :imm-kind :imm8}])

; Instruction forms: each mnemonic maps to encoding forms.
(def instruction-forms-base
  {'nop [{:operands [] :encoding :zo :opcode-map :default :opcode 0x90}]
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
         {:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0xC7 :opcode-ext 0 :rexw true :imm-kind :imm32-sext64}
         {:operands [:r8 :imm8] :encoding :oi :opcode-map :default :opcode 0xB0 :imm-kind :imm8}
         {:operands [:r16 :imm16] :encoding :oi :opcode-map :default :opcode 0xB8 :opsize-prefix true :imm-kind :imm16}
         {:operands [:r32 :imm32] :encoding :oi :opcode-map :default :opcode 0xB8 :imm-kind :imm32}
         {:operands [:r64 :imm64] :encoding :oi :opcode-map :default :opcode 0xB8 :rexw true :imm-kind :imm64}]
   'lea [{:operands [:r16 :m16] :encoding :rm :opcode-map :default :opcode 0x8D :opsize-prefix true}
         {:operands [:r32 :m32] :encoding :rm :opcode-map :default :opcode 0x8D}
         {:operands [:r64 :m64] :encoding :rm :opcode-map :default :opcode 0x8D :rexw true}]
   'movzx [{:operands [:r16 :rm8] :encoding :rm :opcode-map :0f :opcode 0xB6 :opsize-prefix true}
           {:operands [:r32 :rm8] :encoding :rm :opcode-map :0f :opcode 0xB6}
           {:operands [:r64 :rm8] :encoding :rm :opcode-map :0f :opcode 0xB6 :rexw true}
           {:operands [:r16 :rm16] :encoding :rm :opcode-map :0f :opcode 0xB7 :opsize-prefix true}
           {:operands [:r32 :rm16] :encoding :rm :opcode-map :0f :opcode 0xB7}
           {:operands [:r64 :rm16] :encoding :rm :opcode-map :0f :opcode 0xB7 :rexw true}]
   'movsx [{:operands [:r16 :rm8] :encoding :rm :opcode-map :0f :opcode 0xBE :opsize-prefix true}
           {:operands [:r32 :rm8] :encoding :rm :opcode-map :0f :opcode 0xBE}
           {:operands [:r64 :rm8] :encoding :rm :opcode-map :0f :opcode 0xBE :rexw true}
           {:operands [:r16 :rm16] :encoding :rm :opcode-map :0f :opcode 0xBF :opsize-prefix true}
           {:operands [:r32 :rm16] :encoding :rm :opcode-map :0f :opcode 0xBF}
           {:operands [:r64 :rm16] :encoding :rm :opcode-map :0f :opcode 0xBF :rexw true}]
   'movsxd [{:operands [:r64 :rm32] :encoding :rm :opcode-map :default :opcode 0x63 :rexw true}]
   'add (build-group1-forms 0)
   'or (build-group1-forms 1)
   'adc (build-group1-forms 2)
   'sbb (build-group1-forms 3)
   'and (build-group1-forms 4)
   'sub (build-group1-forms 5)
   'xor (build-group1-forms 6)
   'cmp (build-group1-forms 7)
   'test [{:operands [:rm8 :r8] :encoding :mr :opcode-map :default :opcode 0x84}
          {:operands [:rm16 :r16] :encoding :mr :opcode-map :default :opcode 0x85 :opsize-prefix true}
          {:operands [:rm32 :r32] :encoding :mr :opcode-map :default :opcode 0x85}
          {:operands [:rm64 :r64] :encoding :mr :opcode-map :default :opcode 0x85 :rexw true}
          {:operands [:rm8 :imm8] :encoding :mi :opcode-map :default :opcode 0xF6 :opcode-ext 0 :imm-kind :imm8}
          {:operands [:rm16 :imm16] :encoding :mi :opcode-map :default :opcode 0xF7 :opcode-ext 0 :opsize-prefix true :imm-kind :imm16}
          {:operands [:rm32 :imm32] :encoding :mi :opcode-map :default :opcode 0xF7 :opcode-ext 0 :imm-kind :imm32}
          {:operands [:rm64 :imm32] :encoding :mi :opcode-map :default :opcode 0xF7 :opcode-ext 0 :rexw true :imm-kind :imm32}]
   'rol (build-shift-forms 0)
   'ror (build-shift-forms 1)
   'rcl (build-shift-forms 2)
   'rcr (build-shift-forms 3)
   'shl (build-shift-forms 4)
   'shr (build-shift-forms 5)
   'sar (build-shift-forms 7)
   'bt (build-bt-forms 4)
   'bts (build-bt-forms 5)
   'btr (build-bt-forms 6)
   'btc (build-bt-forms 7)
   'push [{:operands [:r64] :encoding :o :opcode-map :default :opcode 0x50}
          {:operands [:imm8] :encoding :i :opcode-map :default :opcode 0x6A :imm-kind :imm8}
          {:operands [:imm32] :encoding :i :opcode-map :default :opcode 0x68 :imm-kind :imm32}
          {:operands [:rm64] :encoding :m :opcode-map :default :opcode 0xFF :opcode-ext 6}]
   'pop [{:operands [:r64] :encoding :o :opcode-map :default :opcode 0x58}
         {:operands [:rm64] :encoding :m :opcode-map :default :opcode 0x8F :opcode-ext 0}]
   'ret [{:operands [] :encoding :zo :opcode-map :default :opcode 0xC3}]
   'call [{:operands [:rel32] :encoding :d :opcode-map :default :opcode 0xE8 :imm-kind :rel32 :fixup-kind :call-rel32}
          {:operands [:rm64] :encoding :m :opcode-map :default :opcode 0xFF :opcode-ext 2}]
   'jmp [{:operands [:rel8] :encoding :d :opcode-map :default :opcode 0xEB :imm-kind :rel8}
         {:operands [:rel32] :encoding :d :opcode-map :default :opcode 0xE9 :imm-kind :rel32 :fixup-kind :jmp-rel32}
         {:operands [:rm64] :encoding :m :opcode-map :default :opcode 0xFF :opcode-ext 4}]
   'imul [{:operands [:r16 :rm16] :encoding :rm :opcode-map :0f :opcode 0xAF :opsize-prefix true}
          {:operands [:r32 :rm32] :encoding :rm :opcode-map :0f :opcode 0xAF}
          {:operands [:r64 :rm64] :encoding :rm :opcode-map :0f :opcode 0xAF :rexw true}
          {:operands [:r16 :rm16 :imm16] :encoding :rmi :opcode-map :default :opcode 0x69 :opsize-prefix true :imm-kind :imm16}
          {:operands [:r32 :rm32 :imm32] :encoding :rmi :opcode-map :default :opcode 0x69 :imm-kind :imm32}
          {:operands [:r64 :rm64 :imm32] :encoding :rmi :opcode-map :default :opcode 0x69 :rexw true :imm-kind :imm32}
          {:operands [:r16 :rm16 :simm8] :encoding :rmi :opcode-map :default :opcode 0x6B :opsize-prefix true :imm-kind :simm8}
          {:operands [:r32 :rm32 :simm8] :encoding :rmi :opcode-map :default :opcode 0x6B :imm-kind :simm8}
          {:operands [:r64 :rm64 :simm8] :encoding :rmi :opcode-map :default :opcode 0x6B :rexw true :imm-kind :simm8}]
   'idiv [{:operands [:rm8] :encoding :m :opcode-map :default :opcode 0xF6 :opcode-ext 7}
          {:operands [:rm16] :encoding :m :opcode-map :default :opcode 0xF7 :opcode-ext 7 :opsize-prefix true}
          {:operands [:rm32] :encoding :m :opcode-map :default :opcode 0xF7 :opcode-ext 7}
          {:operands [:rm64] :encoding :m :opcode-map :default :opcode 0xF7 :opcode-ext 7 :rexw true}]
   'cbw [{:operands [] :encoding :zo :opcode-map :default :opcode 0x98 :opsize-prefix true}]
   'cwde [{:operands [] :encoding :zo :opcode-map :default :opcode 0x98}]
   'cdqe [{:operands [] :encoding :zo :opcode-map :default :opcode 0x98 :rexw true}]
   'cwd [{:operands [] :encoding :zo :opcode-map :default :opcode 0x99 :opsize-prefix true}]
   'cdq [{:operands [] :encoding :zo :opcode-map :default :opcode 0x99}]
   'cqo [{:operands [] :encoding :zo :opcode-map :default :opcode 0x99 :rexw true}]})

(def instruction-forms
  (let [forms {}]
    (for [[mnemonic form-list] instruction-forms-base]
      (put forms mnemonic form-list))
    (for [[mnemonic cc] jcc-mnemonic-nibbles]
      (put forms mnemonic
           [{:operands [:rel8]
             :encoding :d
             :opcode-map :default
             :opcode (+ 0x70 cc)
             :imm-kind :rel8}
            {:operands [:rel32]
             :encoding :d
             :opcode-map :0f
             :opcode (+ 0x80 cc)
             :imm-kind :rel32
             :fixup-kind :jcc-rel32}]))
    (for [[mnemonic cc] setcc-mnemonic-nibbles]
      (put forms mnemonic
           [{:operands [:rm8]
             :encoding :m
             :opcode-map :0f
             :opcode (+ 0x90 cc)
             :opcode-ext 0}]))
    forms))

(def scale-encodings
  {1 0
   2 1
   4 2
   8 3})

(defn get-register-info
  "Return register metadata for a register symbol or raise for unsupported names."
  [reg]
  (or (get register-infos reg)
      (raise 'x86_64 "unsupported register: %s" reg)))

(defn register-name?
  "Return true when v is a known register symbol."
  [v]
  (contains? register-infos v))

(defn reg-code-low3
  "Extract the low three bits of an encoded register number."
  [code]
  (bit-and code 0x7))

(defn reg-code-rex-bit
  "Extract the high register bit used by REX fields."
  [code]
  (bit-and (bit-shift-right code 3) 0x1))

(defn encode-rex
  "Encode a REX prefix byte from w/r/x/b flag bits."
  [w r x b]
  (bit-or 0x40
          (if (nonzero? w) 0x08 0)
          (if (nonzero? r) 0x04 0)
          (if (nonzero? x) 0x02 0)
          (if (nonzero? b) 0x01 0)))

(defn encode-modrm
  "Encode a ModR/M byte from mod, reg, and rm fields."
  [mod reg rm]
  (bit-or (bit-shift-left mod 6)
          (bit-shift-left reg 3)
          rm))

(defn encode-sib
  "Encode a SIB byte from scale, index, and base fields."
  [scale index base]
  (bit-or (bit-shift-left scale 6)
          (bit-shift-left index 3)
          base))

(def immediate-kind-widths
  {:imm8 1
   :simm8 1
   :imm16 2
   :imm32 4
   :imm32-sext64 4
   :imm64 8
   :rel8 1
   :rel32 4})

(def fixup-kind-specs
  {:call-rel32 {:width 4 :relative true}
   :jmp-rel32 {:width 4 :relative true}
   :jcc-rel32 {:width 4 :relative true}
   :rip-disp32 {:width 4 :relative true}
   :abs64 {:width 8 :relative false}})

(defn encode-imm32-le
  "Encode a signed 32-bit immediate as little-endian bytes."
  [imm]
  (if (or (< imm -0x80000000) (> imm 0x7fffffff))
    (raise 'x86_64 "imm32 out of range: %s" imm))
  (let [value (if (< imm 0) (+ 0x100000000 imm) imm)
        b0 (bit-and value 0xFF)
        x1 (bit-shift-right value 8)
        b1 (bit-and x1 0xFF)
        x2 (bit-shift-right x1 8)
        b2 (bit-and x2 0xFF)
        x3 (bit-shift-right x2 8)
        b3 (bit-and x3 0xFF)]
    [b0 b1 b2 b3]))

(defn encode-imm8
  "Validate and encode an unsigned 8-bit immediate."
  [imm]
  (if (or (< imm 0) (> imm 0xFF))
    (raise 'x86_64 "imm8 out of range: %s" imm))
  imm)

(defn encode-imm16-le
  "Encode a 16-bit immediate as little-endian bytes."
  [imm]
  (if (or (< imm -0x8000) (> imm 0xFFFF))
    (raise 'x86_64 "imm16 out of range: %s" imm))
  (let [value (if (< imm 0) (+ 0x10000 imm) imm)
        b0 (bit-and value 0xFF)
        x1 (bit-shift-right value 8)
        b1 (bit-and x1 0xFF)]
    [b0 b1]))

(defn encode-imm64-le
  "Encode a signed 64-bit immediate as little-endian bytes."
  [imm]
  (if (or (< imm -0x8000000000000000) (> imm 0x7fffffffffffffff))
    (raise 'x86_64 "imm64 out of range: %s" imm))
  (let [value (if (< imm 0) (+ 0x10000000000000000 imm) imm)
        b0 (bit-and value 0xFF)
        x1 (bit-shift-right value 8)
        b1 (bit-and x1 0xFF)
        x2 (bit-shift-right x1 8)
        b2 (bit-and x2 0xFF)
        x3 (bit-shift-right x2 8)
        b3 (bit-and x3 0xFF)
        x4 (bit-shift-right x3 8)
        b4 (bit-and x4 0xFF)
        x5 (bit-shift-right x4 8)
        b5 (bit-and x5 0xFF)
        x6 (bit-shift-right x5 8)
        b6 (bit-and x6 0xFF)
        x7 (bit-shift-right x6 8)
        b7 (bit-and x7 0xFF)]
    [b0 b1 b2 b3 b4 b5 b6 b7]))

(defn encode-simm8
  "Encode a signed 8-bit immediate into its byte value."
  [imm]
  (if (or (< imm -128) (> imm 127))
    (raise 'x86_64 "simm8 out of range: %s" imm))
  (if (< imm 0) (+ 0x100 imm) imm))

(defn fill-bytes
  "Return a vector containing count copies of byte."
  [count byte]
  (let [result nil
        n count]
    (while (> n 0)
      (setq result (cons byte result))
      (decq n))
    (Vector result)))

(defn zero-bytes
  "Return a vector containing count zero bytes."
  [count]
  (fill-bytes count 0))

(defn validate-byte
  "Validate that byte is an integer in the 0..255 range."
  [byte]
  (unless (and (integer? byte) (>= byte 0) (< byte 0xFF))
    (raise 'x86_64 "byte out of range: %s" byte))
  byte)

(defn normalize-byte-seq
  "Normalize any iterable of bytes into a validated vector."
  [bytes]
  (Vector (map validate-byte bytes)))

(defn parse-symbol-ref
  "Parse Symbol or symbol-ref maps into validated :symref operand maps.
Returns nil for other input shapes; malformed symbol-ref maps raise."
  [value]
  (cond (symbol? value)
        {:kind :symref :symbol value :addend 0}
        (map? value)
        (let [kind (:kind value)
              sym (:symbol value)
              addend (or (:addend value) 0)]
          (if (not= kind :symref)
            (raise 'x86_64 "not a symbol ref: %s" value))
          (if (nil? sym)
            (raise 'x86_64 "symbol ref requires :symbol: %s" value))
          (if (not (symbol? sym))
            (raise 'x86_64 "symbol ref expects Symbol, got: %s" sym))
          (if (not (integer? addend))
            (raise 'x86_64 "symbol ref addend expects Integer, got: %s" addend))
          {:kind :symref :symbol sym :addend addend})))

(defn possible-symbol-ref?
  "Return true when value has a shape that may parse as a symbol ref."
  [value]
  (or (symbol? value)
      (and (map? value)
           (= (:kind value) :symref))))

(defn mem-from-opts
  "Build a :mem operand map from alternating keyword/value options."
  [opts]
  (let [m (kvs->map opts)]
    (put m :kind :mem)
    m))

(defn mem
  "Construct a memory operand map from key/value pairs."
  [& rest]
  (mem-from-opts rest))

(defn mem-operand?
  "Return true when operand is a memory operand map."
  [operand]
  (and (map? operand)
       (= (:kind operand) :mem)))

(defn possible-mem-operand?
  "Return true when operand has a shape that may parse as a mem operand."
  [operand]
  (case operand
    (Map m) (= (:kind m) :mem)
    `(mem ,@opts) true
    false))

(defn validate-mem-operand
  "Validate required fields and size constraints for a memory operand."
  [operand]
  (if (not (contains? operand :size))
    (raise 'x86_64 "mem operand requires explicit :size: %s" operand))
  (let [size (:size operand)]
    (unless (and (integer? size)
                 (or (= size 1) (= size 2) (= size 4) (= size 8)))
      (raise 'x86_64 "mem operand size should be one of 1/2/4/8, got: %s" size)))
  operand)

(defn parse-mem-operand
  "Parse Map and `(mem ...)` forms into validated memory operands."
  [operand]
  (case operand
    (Map m) (if (= (:kind m) :mem)
              (validate-mem-operand m)
              (raise 'x86_64 "not a mem operand: %s" operand))
    `(mem ,@opts) (validate-mem-operand (mem-from-opts opts))
    operand))

(defn encode-mem
  "Encode a memory operand into ModR/M fields, tail bytes, and fixups."
  [memop]
  (let [{:keys [base index (scale 1) (disp 0) rip]} memop
        rip-ref (if rip (parse-symbol-ref rip))]
    (if (and base (not (symbol? base)))
      (raise 'x86_64 "base should be a Symbol, got: %s" base))
    (if (and index (not (symbol? index)))
      (raise 'x86_64 "index should be a Symbol, got: %s" index))
    (if (not (integer? disp))
      (raise 'x86_64 "disp should be an Integer, got: %s" disp))
    (if (and rip (not (or (integer? rip) rip-ref)))
      (raise 'x86_64
             "rip should be an Integer displacement or symbol ref, got: %s"
             rip))
    (if (and (nil? index) (not= scale 1))
      (raise 'x86_64 "scale requires an index register"))

    (if-let [scale-bits (get scale-encodings scale)]
      (if rip
        (do
          (if (or base index)
            (raise 'x86_64
                   "rip-relative addressing cannot include :base or :index"))
          {:mod 0
           :rm 5
           :rexx 0
           :rexb 0
           :tail (if rip-ref (zero-bytes 4) (encode-imm32-le rip))
           :fixups (if rip-ref
                     [{:kind :rip-disp32
                       :symbol (:symbol rip-ref)
                       :addend (:addend rip-ref)
                       :width 4
                       :relative true
                       :at 0
                       :from 4}]
                     [])})
        (let [no-base? (nil? base)
              [base-low base-high] (if no-base?
                                     [5 0]
                                     (let [baseinfo (get-register-info base)
                                           base-code (if (= (:size baseinfo) 8)
                                                       (:code baseinfo)
                                                       (raise 'x86_64
                                                              "unsupported register: %s"
                                                              base))]
                                       [(reg-code-low3 base-code)
                                        (reg-code-rex-bit base-code)]))
              [index-low index-high] (if index
                                       (let [indexinfo (get-register-info index)
                                             index-code (if (= (:size indexinfo) 8)
                                                          (:code indexinfo)
                                                          (raise 'x86_64
                                                                 "unsupported register: %s"
                                                                 index))
                                             index-low (reg-code-low3 index-code)]
                                         (if (= index-low 4)
                                           (raise 'x86_64
                                                  "rsp/r12 cannot be used as index"))
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
           :tail (catv sib-bytes disp-bytes)
           :fixups []}))
      (raise 'x86_64 "unsupported scale: %s" scale))))

(def operand-ref-indexes
  {:op0 0
   :op1 1
   :op2 2
   :op3 3})

(defn imm-fits-size?
  "Return true when imm fits in the given size, optionally signed."
  [size imm &opt (signed false)]
  (case size
    1 (if signed
        (and (>= imm -128) (<= imm 127))
        (and (>= imm 0) (<= imm 0xFF)))
    2 (if signed
        (and (>= imm -0x8000) (<= imm 0x7fff))
        (and (>= imm -0x8000) (<= imm 0xFFFF)))
    4 (and (>= imm -0x80000000) (<= imm 0x7fffffff))
    8 true
    false))

(defn encode-immediate-kind
  "Encode an immediate by semantic kind (imm/rel variants)."
  [imm-kind imm]
  (case imm-kind
    :imm8 [(encode-imm8 imm)]
    :simm8 [(encode-simm8 imm)]
    :imm16 (encode-imm16-le imm)
    :imm32 (encode-imm32-le imm)
    :imm32-sext64 (encode-imm32-le imm)
    :imm64 (encode-imm64-le imm)
    :rel8 [(encode-simm8 imm)]
    :rel32 (encode-imm32-le imm)
    (raise 'x86_64
           "unsupported immediate encoding kind: %s"
           imm-kind)))

(defn normalize-operand
  "Normalize raw operands into tagged internal operand representations."
  [operand]
  (cond (and (symbol? operand) (register-name? operand))
        {:kind :reg :reg operand :reginfo (get-register-info operand)}
        (integer? operand)
        {:kind :imm :value operand}
        (possible-mem-operand? operand)
        (parse-mem-operand operand)
        (possible-symbol-ref? operand)
        (parse-symbol-ref operand)
        {:kind :other :value operand}))

(defn match-operand-class
  "Try to match a normalized operand against an operand class spec."
  [operand class-key]
  (let [spec (get operand-class-specs class-key)]
    (if (nil? spec)
      (raise 'x86_64 "unknown operand class: %s" class-key))
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
              (let [mem-size (:size operand)]
                (if (and size mem-size (not= size mem-size))
                  nil
                  operand))))))
      :imm
      (if (and (= (:kind operand) :imm)
               (if (:size spec)
                 (imm-fits-size? (:size spec) (:value operand) (:signed spec))
                 true))
        operand)
      :rel
      (if (and (= (:kind operand) :imm)
               (if (:size spec)
                 (imm-fits-size? (:size spec) (:value operand) (:signed spec))
                 true))
        {:kind :rel :value (:value operand)}
        (if (and (= (:kind operand) :symref)
                 (or (nil? (:size spec)) (= (:size spec) 4)))
          operand))
      :moffs
      (if (= (:kind operand) :mem)
        operand)
      nil)))

(defn select-form-for-operands
  "Choose the first instruction form that matches normalized operands."
  [forms normalized-operands]
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

(defn get-operand-by-ref
  "Resolve operand references like :op0/:op1 from prepared operands."
  [prepared-operands opref]
  (if-match [(Integer idx) (get operand-ref-indexes opref)]
    (get prepared-operands idx)
    (raise 'x86_64 "invalid operand reference: %s" opref)))

(defn encode-rex-with-flags
  "Emit a REX byte when needed and reject invalid high-8 register mixes."
  [w r x b rex-required high8]
  (let [rex-needed (or (nonzero? w)
                       (nonzero? r)
                       (nonzero? x)
                       (nonzero? b)
                       rex-required)]
    (if (and high8 rex-needed)
      (raise 'x86_64
             "high 8-bit registers (ah/ch/dh/bh) cannot be encoded with REX"))
    (if rex-needed
      (encode-rex w r x b)
      nil)))

(defn collect-rex-register-flags
  "Collect aggregate REX requirements from register operands."
  [prepared-operands]
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

(defn patch-bytes
  "Return bytes with patch written at offset at."
  [bytes at patch]
  (let [bytes-len (len bytes)
        patch-len (len patch)]
    (if (or (< at 0) (> (+ at patch-len) bytes-len))
      (raise 'x86_64
             "patch out of bounds at=%s width=%s bytes=%s"
             at
             patch-len
             bytes-len))
    (let [result nil
          idx 0
          it (iter bytes)]
      (while it
        (let [value (if (and (>= idx at) (< idx (+ at patch-len)))
                      (get patch (- idx at))
                      @it)]
          (setq result (cons value result))
          (incq idx)
          (setq it (next it))))
      (Vector (nreverse result)))))

(defn relocate-fixup
  "Shift a fixup's offsets by a byte offset."
  [fixup offset]
  {:kind (:kind fixup)
   :symbol (:symbol fixup)
   :addend (or (:addend fixup) 0)
   :width (:width fixup)
   :relative (:relative fixup)
   :at (+ offset (:at fixup))
   :from (+ offset (:from fixup))})

(defn emit-form-encoding
  "Encode one selected instruction form and return bytes with fixups."
  [form prepared-operands]
  (let [pattern (get encoding-patterns (:encoding form))]
    (if (nil? pattern)
      (raise 'x86_64 "unknown encoding pattern: %s" (:encoding form)))
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
                     :tail []
                     :fixups []}
                    (let [memenc (encode-mem rm-op)]
                      {:mod (:mod memenc)
                       :rm-low (:rm memenc)
                       :rexx (:rexx memenc)
                       :rexb (:rexb memenc)
                       :tail (:tail memenc)
                       :fixups (:fixups memenc)})))
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
          rex-bytes (if rex [rex] [])
          modrm-bytes (if modrm-spec [modrm-byte] [])
          tail-bytes (if modrm-spec (:tail rmenc) [])
          before-tail-bytes (catv prefix-bytes
                                  rex-bytes
                                  opcode-map-prefix
                                  [opcode]
                                  modrm-bytes)
          tail-fixups (if modrm-spec
                        (mapv (fn [fixup]
                                (relocate-fixup fixup (len before-tail-bytes)))
                              (:fixups rmenc))
                        [])
          before-imm-bytes (catv before-tail-bytes tail-bytes)
          imm-ref (or (:imm pattern) (:rel pattern))
          imm-encoding (if imm-ref
                         (let [imm-op (get-operand-by-ref prepared-operands imm-ref)
                               imm-index (get operand-ref-indexes imm-ref)
                               default-imm-kind (get (:operands form) imm-index)
                               imm-kind (or (:imm-kind form) default-imm-kind)
                               imm-width (get immediate-kind-widths imm-kind)]
                           (if (= (:kind imm-op) :symref)
                             (if (nil? imm-width)
                               (raise 'x86_64 "unknown immediate width for: %s" imm-kind)
                               {:bytes (zero-bytes imm-width)
                                :fixups [{:kind (or (:fixup-kind form) :rel32)
                                          :symbol (:symbol imm-op)
                                          :addend (:addend imm-op)
                                          :width imm-width
                                          :relative true
                                          :at (len before-imm-bytes)
                                          :from (+ (len before-imm-bytes) imm-width)}]})
                             {:bytes (encode-immediate-kind imm-kind (:value imm-op))
                              :fixups []}))
                         {:bytes [] :fixups []})
          bytes (catv before-imm-bytes (:bytes imm-encoding))
          fixups (catv tail-fixups (:fixups imm-encoding))]
      {:bytes bytes
       :fixups fixups})))

(defn encode-instruction-form-driven-detailed
  "Encode one instruction using form lookup and operand matching."
  [instruction]
  (if-match [[(Symbol raw-mnemonic) & operands] instruction]
    (if-let [mnemonic (or (get mnemonic-aliases raw-mnemonic) raw-mnemonic)
             forms (get instruction-forms mnemonic)
             normalized-operands (mapv normalize-operand operands)
             {:keys [form operands]} (select-form-for-operands forms normalized-operands)]
      (emit-form-encoding form operands))))

(defn encode-instruction-detailed
  "Encode one instruction and raise when unsupported."
  [instruction]
  (or (encode-instruction-form-driven-detailed instruction)
      (raise 'x86_64 "unsupported instruction: %s" instruction)))

(defn encode-instruction
  "Encode one instruction and return only encoded bytes."
  [instruction]
  (:bytes (encode-instruction-detailed instruction)))

(defn asm-new
  "Create a new assembler state map."
  [&opt (options {})]
  (let [mode (or (:mode options) :64)
        segment-order [:text :rodata :data]
        segments {}]
    (if (not= mode :64)
      (raise 'x86_64 "unsupported asm mode: %s" mode))
    (for [segment segment-order]
      (put segments segment []))
    {:mode mode
     :segment :text
     :segment-order segment-order
     :segments segments
     :labels {}
     :symbols (or (:symbols options) {})
     :fixups []
     :diagnostics []}))

(defn ensure-asm-segment!
  "Validate that segment exists in asm and return it."
  [asm segment]
  (if (not (keyword? segment))
    (raise 'x86_64 "segment should be a Keyword, got: %s" segment))
  (if (not (contains? (:segments asm) segment))
    (raise 'x86_64 "unknown segment: %s" segment))
  segment)

(defn asm-segment!
  "Switch the active output segment on asm."
  [asm segment]
  (ensure-asm-segment! asm segment)
  (put asm :segment segment)
  asm)

(defn asm-segment-bytes
  "Return bytes currently emitted for segment."
  [asm segment]
  (or (get (:segments asm) segment) []))

(defn asm-segment-offset
  "Return current byte offset within segment."
  [asm segment]
  (len (asm-segment-bytes asm segment)))

(defn asm-append-bytes!
  "Append validated bytes to a segment and return previous offset."
  [asm segment bytes]
  (ensure-asm-segment! asm segment)
  (let [segments (:segments asm)
        normalized (normalize-byte-seq bytes)
        offset (len (or (get segments segment) []))]
    (put segments segment (catv (or (get segments segment) []) normalized))
    offset))

(defn asm-bytes!
  "Append bytes to the active (or provided) segment."
  [asm bytes &opt segment]
  (let [active (or segment (:segment asm))]
    (asm-append-bytes! asm active bytes)
    asm))

(defn asm-label!
  "Define a label at the current segment offset."
  [asm name &opt segment]
  (if (not (symbol? name))
    (raise 'x86_64 "label name should be a Symbol, got: %s" name))
  (if segment
    (asm-segment! asm segment))
  (let [labels (:labels asm)
        active (:segment asm)]
    (if (contains? labels name)
      (raise 'x86_64 "duplicate label: %s" name))
    (put labels name
         {:segment active
          :offset (asm-segment-offset asm active)})
    asm))

(defn asm-add-fixup!
  "Append a fixup entry to asm."
  [asm fixup]
  (put asm :fixups (catv (:fixups asm) [fixup]))
  asm)

(defn asm-emit!
  "Encode and emit one instruction, recording generated fixups."
  [asm instruction &opt segment]
  (if segment
    (asm-segment! asm segment))
  (let [active (:segment asm)
        start (asm-segment-offset asm active)
        encoded (encode-instruction-detailed instruction)]
    (asm-append-bytes! asm active (:bytes encoded))
    (for [fixup (:fixups encoded)]
      (asm-add-fixup! asm
                      {:kind (:kind fixup)
                       :symbol (:symbol fixup)
                       :segment active
                       :at (+ start (:at fixup))
                       :from (+ start (:from fixup))
                       :width (:width fixup)
                       :relative (:relative fixup)
                       :addend (or (:addend fixup) 0)}))
    asm))

(defn asm-align!
  "Pad the active segment to an alignment boundary."
  [asm to &opt (fill 0x00) segment]
  (if (not (integer? to))
    (raise 'x86_64 "align boundary should be Integer, got: %s" to))
  (if (<= to 0)
    (raise 'x86_64 "align boundary should be > 0, got: %s" to))
  (if segment
    (asm-segment! asm segment))
  (let [active (:segment asm)
        offset (asm-segment-offset asm active)
        remainder (mod offset to)
        padding (if (= remainder 0) 0 (- to remainder))]
    (if (> padding 0)
      (asm-append-bytes! asm active (fill-bytes padding fill)))
    asm))

(defn asm-fixup!
  "Reserve bytes and record a manual fixup entry."
  [asm fixup]
  (if (not (map? fixup))
    (raise 'x86_64 "asm-fixup! expects a map, got: %s" fixup))
  (let [kind (:kind fixup)
        spec (or (get fixup-kind-specs kind)
                 (raise 'x86_64 "unsupported fixup kind: %s" kind))
        symref (or (parse-symbol-ref {:kind :symref
                                      :symbol (:symbol fixup)
                                      :addend (if (contains? fixup :addend) (:addend fixup) 0)})
                   (raise 'x86_64 "fixup expects :symbol, got: %s" fixup))
        active (or (:segment fixup) (:segment asm))
        start (asm-append-bytes! asm active (zero-bytes (:width spec)))]
    (asm-add-fixup! asm
                    {:kind kind
                     :symbol (:symbol symref)
                     :segment active
                     :at start
                     :from (+ start (:width spec))
                     :width (:width spec)
                     :relative (:relative spec)
                     :addend (:addend symref)})
    asm))

(defn normalize-asm-item
  "Validate and normalize one module item into internal form."
  [item]
  (if (not (map? item))
    (raise 'x86_64 "asm item should be a map, got: %s" item))
  (let [op (:op item)]
    (case op
      :label (do
               (if (not (symbol? (:name item)))
                 (raise 'x86_64 "label item expects :name Symbol, got: %s" item))
               {:op :label
                :name (:name item)
                :segment (or (:segment item) :text)})
      :instruction (do
             (if (not (cons? (:form item)))
               (raise 'x86_64 "instruction item expects :form instruction list, got: %s" item))
             {:op :instruction
              :segment (or (:segment item) :text)
              :form (:form item)})
      :bytes {:op :bytes
              :segment (or (:segment item) :text)
              :value (:value item)}
      :align {:op :align
              :segment (or (:segment item) :text)
              :to (:to item)
              :fill (if (contains? item :fill) (:fill item) 0x00)}
      :fixup {:op :fixup
            :segment (or (:segment item) :text)
            :kind (:kind item)
            :symbol (:symbol item)
            :addend (if (contains? item :addend) (:addend item) 0)}
      (raise 'x86_64 "unsupported asm item op: %s" op))))

(defn normalize-asm-module
  "Validate and normalize a high-level assembly module map."
  [module]
  (if (not (map? module))
    (raise 'x86_64 "module should be a map, got: %s" module))
  (let [mode (or (:mode module) :64)
        items (or (:items module) [])
        options (or (:options module) {})]
    (if (not= mode :64)
      (raise 'x86_64 "unsupported module mode: %s" mode))
    {:mode mode
     :items (mapv normalize-asm-item items)
     :symbols (or (:symbols module) {})
     :options options}))

(defn asm-apply-item!
  "Apply one normalized item to assembler state."
  [asm item]
  (case (:op item)
    :label (asm-label! asm (:name item) (:segment item))
    :instruction (asm-emit! asm (:form item) (:segment item))
    :bytes (asm-bytes! asm (:value item) (:segment item))
    :align (asm-align! asm (:to item) (:fill item) (:segment item))
    :fixup (asm-fixup! asm item)
    (raise 'x86_64 "unsupported normalized item op: %s" (:op item))))

(defn build-segment-layout
  "Compute absolute offsets and lengths for each segment."
  [segment-order segments]
  (let [layout {}
        offset 0]
    (for [segment segment-order]
      (let [bytes (or (get segments segment) [])
            length (len bytes)]
        (put layout segment {:offset offset :length length})
        (setq offset (+ offset length))))
    layout))

(defn build-label-layout
  "Compute absolute label offsets from per-segment label positions."
  [labels segment-layout]
  (let [result {}]
    (for [[name info] labels]
      (let [segment (:segment info)
            segment-info (or (get segment-layout segment)
                             (raise 'x86_64 "label uses unknown segment: %s" segment))
            offset (:offset info)
            absolute (+ (:offset segment-info) offset)]
        (put result name
             {:segment segment
              :offset offset
              :absolute-offset absolute})))
    result))

(defn resolve-symbol-absolute
  "Resolve a symbol to an absolute offset from labels or externals."
  [sym labels externals]
  (if-let [label (get labels sym)]
    (:absolute-offset label)
    (if (contains? externals sym)
      (get externals sym))))

(defn encode-fixup-value
  "Encode a resolved fixup value using the fixup width/relative rules."
  [fixup value]
  (let [width (:width fixup)]
    (case width
      1 [(if (:relative fixup) (encode-simm8 value) (encode-imm8 value))]
      2 (encode-imm16-le value)
      4 (encode-imm32-le value)
      8 (encode-imm64-le value)
      (raise 'x86_64 "unsupported fixup width: %s" width))))

(defn asm-finalize
  "Resolve fixups and return the finalized assembly artifact."
  [asm &opt (options {})]
  (let [strict (if (contains? options :strict) (:strict options) true)
        externals (or (:externals options) {})
        segment-order (:segment-order asm)
        original-segments (:segments asm)
        segment-layout (build-segment-layout segment-order original-segments)
        labels (build-label-layout (:labels asm) segment-layout)
        patched-segments {}
        unresolved nil
        diagnostics nil
        finalized-fixups nil]
    (for [segment segment-order]
      (put patched-segments segment (Vector (or (get original-segments segment) []))))
    (for [fixup (:fixups asm)]
      (let [sym (:symbol fixup)
            target (resolve-symbol-absolute sym labels externals)
            segment (:segment fixup)
            segment-info (or (get segment-layout segment)
                             (raise 'x86_64 "fixup uses unknown segment: %s" segment))]
        (if (nil? target)
          (do
            (setq unresolved (cons fixup unresolved))
            (setq diagnostics
                  (cons {:level :error
                         :kind :unknown-symbol
                         :symbol sym
                         :fixup fixup}
                        diagnostics))
            (setq finalized-fixups
                  (cons {:kind (:kind fixup)
                         :symbol sym
                         :segment segment
                         :at (:at fixup)
                         :from (:from fixup)
                         :width (:width fixup)
                         :relative (:relative fixup)
                         :addend (:addend fixup)
                         :status :unresolved}
                        finalized-fixups)))
          (let [from-abs (+ (:offset segment-info) (:from fixup))
                value-base (+ target (or (:addend fixup) 0))
                value (if (:relative fixup)
                        (- value-base from-abs)
                        value-base)
                encoded (catch [e {:error e}]
                          {:bytes (encode-fixup-value fixup value)})]
            (if (:error encoded)
              (do
                (setq diagnostics
                      (cons {:level :error
                             :kind :fixup-overflow
                             :symbol sym
                             :fixup fixup
                             :value value
                             :error (:error encoded)}
                            diagnostics))
                (setq finalized-fixups
                      (cons {:kind (:kind fixup)
                             :symbol sym
                             :segment segment
                             :at (:at fixup)
                             :from (:from fixup)
                             :width (:width fixup)
                             :relative (:relative fixup)
                             :addend (:addend fixup)
                             :status :error}
                            finalized-fixups)))
              (let [bytes (:bytes encoded)
                    old-segment-bytes (or (get patched-segments segment) [])]
                (put patched-segments segment
                     (patch-bytes old-segment-bytes (:at fixup) bytes))
                (setq finalized-fixups
                      (cons {:kind (:kind fixup)
                             :symbol sym
                             :segment segment
                             :at (:at fixup)
                             :from (:from fixup)
                             :width (:width fixup)
                             :relative (:relative fixup)
                             :addend (:addend fixup)
                             :value value
                             :status :applied}
                            finalized-fixups))))))))
    (let [artifact {:bytes (reduce (fn [segment acc]
                                     (catv acc (or (get patched-segments segment) [])))
                                   []
                                   segment-order)
                    :segments segment-layout
                    :labels labels
                    :symbols {:declared (:symbols asm)
                              :labels labels
                              :externals externals}
                    :fixups (Vector (nreverse finalized-fixups))
                    :unresolved (Vector (nreverse unresolved))
                    :diagnostics (Vector (nreverse diagnostics))}]
      (if (and strict
               (or (nonzero? (len (:unresolved artifact)))
                   (nonzero? (len (:diagnostics artifact)))))
        (raise 'x86_64
               "asm-finalize failed: unresolved=%s diagnostics=%s"
               (len (:unresolved artifact))
               (len (:diagnostics artifact))))
      artifact)))

(defn assemble-module
  "Assemble a module map end-to-end and return the finalized artifact."
  [module &opt (options {})]
  (let [normalized (normalize-asm-module module)
        asm (asm-new {:mode (:mode normalized)
                      :symbols (:symbols normalized)})
        module-options (:options normalized)]
    (for [item (:items normalized)]
      (asm-apply-item! asm item))
    (asm-finalize asm
                  {:strict (if (contains? options :strict)
                             (:strict options)
                             (if (contains? module-options :strict)
                               (:strict module-options)
                               true))
                   :externals (if (contains? options :externals)
                                (:externals options)
                                (or (:externals module-options) {}))})))
