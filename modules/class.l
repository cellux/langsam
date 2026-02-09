(defn enum?
  [spec]
  (and (vector? spec)
       (> (len spec) 0)
       (= (get spec 0) :enum)))

(defn enum-contains?
  [spec value]
  (let [i 1
        found false]
    (while (and (not found) (< i (len spec)))
      (if (= (get spec i) value)
        (setq found true)
        (incq i)))
    found))

(defn coerce-value
  [class-name field spec value]
  (cond
   (nil? value) nil
   (type? spec)
   (if (= (type value) spec)
     value
     (raise 'class "%s.%s expects %s, got %s"
            class-name field spec (type value)))
   (class? spec)
   (spec value)
   (enum? spec)
   (if (enum-contains? spec value)
     value
     (raise 'class "%s.%s expects one of %s, got %s"
            class-name field spec value))
   (raise 'class "%s.%s has unsupported schema type: %s"
          class-name field spec)))

(defn compile-validators
  [opts]
  (if (contains? opts :validate)
    (let [validate (:validate opts)]
      (cond
       (function? validate) [validate]
       (vector? validate)
       (do
         (for [validator validate]
           (if (not (function? validator))
             (raise 'class
                    "class field :validate entries should be Function, got %s"
                    (type validator))))
         validate)
       (raise 'class "class field :validate should be Function or Vector, got %s"
              (type validate))))
    []))

(defn run-validators
  [class-name field validators value]
  (for [validator validators]
    (if (not (validator value))
      (raise 'class "%s.%s failed validator" class-name field))))

(defn compile-field
  [field]
  (if (not (vector? field))
    (raise 'class "class field schema should be Vector, got %s" field))
  (let [n (len field)]
    (if (or (< n 2) (> n 3))
      (raise 'class
             "class field schema expects [field type &opt opts], got %s"
             field))
    (let [[field-name spec &opt (opts {})] field]
      (if (not (keyword? field-name))
        (raise 'class "class field name should be Keyword, got %s" field-name))
      (if (not (map? opts))
        (raise 'class "class field options should be Map, got %s" opts))
      (let [validators (compile-validators opts)]
        {:name field-name
         :spec spec
         :required (if (contains? opts :required) (:required opts) false)
         :validators validators}))))

(defn compile-schema
  [schema]
  (if (not (vector? schema))
    (raise 'class "class schema should be Vector, got %s" schema))
  (mapv compile-field schema))

(defn construct
  [class input]
  (if (not (map? input))
    (raise 'class "%s constructor expects Map, got %s"
           (%name class) (type input)))
  (let [instance (Map input)
        schema (%schema class)]
    (for [entry schema]
      (let [field (:name entry)
            spec (:spec entry)
            required (:required entry)
            validators (:validators entry)
            present (contains? input field)]
        (if (and required (not present))
          (raise 'class "%s.%s is required" (%name class) field))
        (if present
          (let [value (coerce-value (%name class) field spec (get input field))]
            (run-validators (%name class) field validators value)
            (put instance field value)))))
    (setproto instance class)
    instance))

(defn class
  [name schema]
  (let [compiled-schema (compile-schema schema)
        class {%name name
               %schema compiled-schema}]
    (put class %invoke (fn [self input]
                         (construct self input)))
    class))
