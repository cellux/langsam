; classes are implemented as maps

(defclass Color
  [:r Integer]
  [:g Integer]
  [:b Integer])

(defclass Transmission
  [:type [:enum :manual :automatic :cvt]]
  [:gears Integer])

(defclass Engine
  [:type [:enum :petrol :diesel :electric :hybrid] {:required true}]
  [:cylinders Integer]
  [:power Float]
  [:transmission Transmission])

(defclass Car
  [:vin String]
  [:make String {:required true}]
  [:model String {:required true}]
  [:year Integer]
  [:body-type [:enum :sedan :hatchback :suv :coupe]]
  [:color Color]
  [:doors Integer]
  [:seats Integer]
  [:engine Engine {:required true}])

; defclass defines a constructor function which can be used to turn a
; map into a class instance

(let [car (Car {:make "Toyota"
                :model "Auris"
                :engine {:type :petrol :transmission {:type :manual :gears 5}}})]
  (assert (= (:vin car) nil))
  (assert (= (:make car) "Toyota"))
  (assert (= (:model car) "Auris"))
  (assert (= (:engine car) {:type :petrol :transmission {:type :manual :gears 5}})))

; class lets you create class values on the fly
(let [Point (class Point [:x Integer] [:y Integer])
      p (Point {:x 6 :y -3})]
  (assert (= (:x p) 6))
  (assert (= (:y p) -3)))

; field validators
(defclass Score
  [:value Integer {:required true
                   :validate [(fn [n] (>= n 0))
                              (fn [n] (< n 100))]}]
  [:label String {:validate (fn [s] (> (len s) 0))}])

(let [s (Score {:value 42 :label "ok"})]
  (assert (= (:value s) 42))
  (assert (= (:label s) "ok")))

(assert (throws? '(class . "Score.:value failed validator")
                 (Score {:value -1 :label "ok"})))
(assert (throws? '(class . "Score.:value failed validator")
                 (Score {:value 101 :label "ok"})))
(assert (throws? '(class . "Score.:label failed validator")
                 (Score {:value 10 :label ""})))

; validators run on nil values when the field is present
(defclass RequiredSome
  [:x Integer {:required true :validate some?}])
(assert (throws? '(class . "RequiredSome.:x failed validator")
                 (RequiredSome {:x nil})))

; methods are defined with Class.method syntax
(defclass Counter
  [:value Integer {:required true}])

(defmethod Counter.read
  [self]
  (:value self))

(defmethod Counter.inc
  [self delta]
  (update self :value + delta)
  self)

(let [c (Counter {:value 10})]
  (assert (= (c.read) 10))
  (assert (= (c.inc 5) {:value 15}))
  (assert (= (c.read) 15)))

; methods can be attached to class values in local scope
(let [Point (class LocalPoint [:x Integer] [:y Integer])]
  (defmethod Point.shift
    [self dx dy]
    (update self :x + dx)
    (update self :y + dy)
    self)
  (let [p (Point {:x 1 :y 2})]
    (assert (= (p.shift 3 -5) {:x 4 :y -3}))
    (assert (= (:x p) 4))
    (assert (= (:y p) -3))))

; invalid method target is rejected
(assert (throws? '(syntax . "defmethod expects Class.method target, got bad-target")
                 (defmethod bad-target [self] self)))

; single inheritance
(defclass Vehicle
  [:make String {:required true}]
  [:wheels Integer {:validate (fn [n] (> n 0))}])

(defmethod Vehicle.describe
  [self]
  (str (:make self) ":" (:wheels self)))

(defclass Sedan
  {:extends Vehicle}
  [:model String {:required true}])

(let [s (Sedan {:make "Toyota" :model "Corolla" :wheels 4})]
  (assert (= (:make s) "Toyota"))
  (assert (= (:model s) "Corolla"))
  (assert (= (s.describe) "Toyota:4")))

(assert (throws? '(class . "Sedan.:make is required")
                 (Sedan {:model "Corolla"})))
(assert (throws? '(class . "Sedan.:wheels failed validator")
                 (Sedan {:make "Toyota" :model "Corolla" :wheels 0})))

(defmethod Sedan.describe
  [self]
  (str "sedan:" (:model self)))

(let [s (Sedan {:make "Toyota" :model "Corolla" :wheels 4})]
  (assert (= (s.describe) "sedan:Corolla")))

(assert (throws? '(class . "class :extends should be Class, got Integer")
                 (class BadExtends {:extends 1} [:x Integer])))
(assert (throws? '(class . "BadSedan has duplicate field: :make")
                 (class BadSedan {:extends Vehicle} [:make String])))

; class? recognizes class values
(assert (class? Car))
(assert (not (class? {})))

; class should not leak a binding into the global scope
(assert (not (contains? (curlet) 'ClassMacroEphemeralPoint)))
(let [PointClass (class ClassMacroEphemeralPoint [:x Integer])
      p (PointClass {:x 7})]
  (assert (class? PointClass))
  (assert (= (:x p) 7)))
(assert (not (contains? (curlet) 'ClassMacroEphemeralPoint)))

; required checks key presence, not non-nil value
(let [Req (class Req [:x Integer {:required true}])
      r (Req {:x nil})]
  (assert (contains? r :x))
  (assert (= (:x r) nil)))

; nested class coercion accepts nested maps and class instances
(let [gearbox (Transmission {:type :manual :gears 6})
      engine (Engine {:type :petrol :transmission gearbox})
      car (Car {:make "Toyota" :model "Corolla" :engine engine})]
  (assert (= (:type (:engine car)) :petrol))
  (assert (= (:transmission (:engine car))
             {:type :manual :gears 6})))

; constructor validation errors
(assert (throws? '(class . "Car constructor expects Map, got Vector")
                 (Car [])))
(assert (throws? '(class . "Car.:make is required")
                 (Car {:model "Auris" :engine {:type :petrol}})))
(assert (throws? '(class . "Engine.:type is required")
                 (Car {:make "Toyota" :model "Auris" :engine {}})))
(assert (throws? '(class . "Car.:year expects Integer, got String")
                 (Car {:make "Toyota"
                       :model "Auris"
                       :year "2020"
                       :engine {:type :petrol}})))
(assert (throws? '(class . "Car.:body-type expects one of [:enum :sedan :hatchback :suv :coupe], got :truck")
                 (Car {:make "Toyota"
                       :model "Auris"
                       :body-type :truck
                       :engine {:type :petrol}})))
(assert (throws? '(class . "Engine.:type expects one of [:enum :petrol :diesel :electric :hybrid], got :steam")
                 (Car {:make "Toyota"
                       :model "Auris"
                       :engine {:type :steam}})))

; schema validation errors
(assert (throws? '(class . "class field schema should be Vector, got :oops")
                 (class BrokenSchema1 :oops)))
(assert (throws? '(class . "class field schema expects [field type &opt opts], got [:x]")
                 (class BrokenSchema2 [:x])))
(assert (throws? '(class . "class field name should be Keyword, got x")
                 (class BrokenSchema3 ['x Integer])))
(assert (throws? '(class . "class field options should be Map, got []")
                 (class BrokenSchema4 [:x Integer []])))
(assert (throws? '(class . "class field :validate should be Function or Vector, got Integer")
                 (class BrokenSchema5 [:x Integer {:validate 1}])))
(assert (throws? '(class . "class field :validate entries should be Function, got Integer")
                 (class BrokenSchema6 [:x Integer {:validate [inc 1]}])))

; unsupported schema type is rejected at construction time
(let [BrokenType (class BrokenType [:x :weird])]
  (assert (throws? '(class . "BrokenType.:x has unsupported schema type: :weird")
                   (BrokenType {:x 1}))))
