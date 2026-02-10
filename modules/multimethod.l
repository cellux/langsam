(defn multimethod?
  [x]
  (and (map? x)
       (%multimethod x)
       (function? (%dispatch x))
       (map? (%methods x))
       (function? (%invoke x))))

(defn resolve-method
  [mm dispatch-value]
  (let [methods (%methods mm)
        method (get methods dispatch-value)]
    (if (function? method)
      method
      (let [default-method (get methods :default)]
        (if (function? default-method)
          default-method
          nil)))))

(defn add-method
  [mm dispatch-value method]
  (if (not (multimethod? mm))
    (raise 'defmethod "defmethod target should be Multimethod, got %s" (type mm)))
  (if (not (function? method))
    (raise 'defmethod "defmethod value should be Function, got %s" (type method)))
  (put (%methods mm) dispatch-value method)
  mm)

(defn multimethod
  [name dispatch-fn]
  (if (not (function? dispatch-fn))
    (raise 'defmulti "defmulti dispatch function should be Function, got %s"
           (type dispatch-fn)))
  (let [mm {%name name
            %multimethod true
            %dispatch dispatch-fn
            %methods {}}]
    (put mm %invoke
         (fn [self & args]
           (let [dispatch-value (apply (%dispatch self) args)
                 method (resolve-method self dispatch-value)]
             (if (nil? method)
               (raise 'invoke "no method in %s for dispatch value %s"
                      (%name self) dispatch-value))
             (apply method args))))
    mm))
