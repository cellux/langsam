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

; class/type helper supports dispatch by class for instances and by type otherwise
(defclass Pet
  [:name String {:required true}])

(let [pet (Pet {:name "luna"})]
  (assert (= (class pet) Pet))
  (assert (= (class 42) Integer))
  (assert (= (class {}) Map)))

(defmulti describe-class-or-type
  class)

(defmethod describe-class-or-type Pet
  [x]
  "pet")

(defmethod describe-class-or-type Integer
  [x]
  "int")

(defmethod describe-class-or-type :default
  [x]
  "other")

(assert (= (describe-class-or-type (Pet {:name "luna"})) "pet"))
(assert (= (describe-class-or-type 7) "int"))
(assert (= (describe-class-or-type true) "other"))

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

; map dispatch keys should not depend on insertion order
(defmulti classify-map
  (fn [x] x))

(let [k1 (Map 2)
      k2 (Map 2)]
  (put k1 0 1)
  (put k1 4 2)
  (put k2 4 2)
  (put k2 0 1)
  (defmethod classify-map k1
    [x]
    :map-hit)
  (assert (= (classify-map k2) :map-hit)))

; defmethod supports multimethod docstrings
(defmulti with-doc-method
  type)

(defmethod with-doc-method Integer
  "Increment integer values."
  [x]
  (+ x 1))

(assert (= (with-doc-method 4) 5))
(let [method (get (%methods with-doc-method) Integer)]
  (assert (= (:doc method) "Increment integer values.")))
