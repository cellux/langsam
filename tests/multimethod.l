; multimethod dispatch by type
(defmulti describe-type
  (fn [x] (type x)))

(defmethod describe-type Integer
  [x]
  (str "int:" x))

(defmethod describe-type String
  [x]
  (str "str:" x))

(defmethod describe-type :default
  [x]
  (str "other:" (type x)))

(assert (= (describe-type 7) "int:7"))
(assert (= (describe-type "a") "str:a"))
(assert (= (describe-type true) "other:Boolean"))

; multimethod dispatch by a composite value
(defmulti combine-kind
  (fn [x y] [(type x) (type y)]))

(defmethod combine-kind [Integer Integer]
  [x y]
  (+ x y))

(defmethod combine-kind [String String]
  [x y]
  (str x y))

(defmethod combine-kind :default
  [x y]
  [x y])

(assert (= (combine-kind 2 3) 5))
(assert (= (combine-kind "a" "b") "ab"))
(assert (= (combine-kind 1 "b") [1 "b"]))

; missing dispatch method without default should raise
(defmulti only-int type)
(defmethod only-int Integer
  [x]
  x)

(assert (throws? '(invoke . "no method in only-int for dispatch value String")
                 (only-int "x")))

; defmulti validation
(assert (throws? '(defmulti . "defmulti dispatch function should be Function, got Integer")
                 (defmulti bad-multi 1)))
(assert (throws? '(syntax . "defmulti name should be Symbol, got :bad-name")
                 (defmulti :bad-name type)))

; defmethod multimethod validation
(assert (throws? '(syntax . "defmethod multimethod params should be Vector, got x")
                 (defmethod describe-type Integer x x)))

(let [not-mm 1]
  (assert (throws? '(defmethod . "defmethod target should be Multimethod, got Integer")
                   (defmethod not-mm :x [x] x))))

; float dispatch keys should respect equality/hash contract
(defmulti classify-float
  (fn [x] x))

(defmethod classify-float 0.0
  [x]
  :zero)

(defmethod classify-float (Float "nan")
  [x]
  :nan)

(assert (= (classify-float -0.0) :zero))
(assert (= (classify-float (Float "-nan")) :nan))
