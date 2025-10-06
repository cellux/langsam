;; (defmacro defmulti
;;   [name dispatch-fn]
;;   `(let [dispatch-map {}]
;;      `(put dispatch-map '%call (fn [& args]
;;                                  (let [dispatch-value (apply ,dispatch-fn args)
;;                                         method (get dispatch-map dispatch-value)]
;;                                    (apply method args))))
;;      dispatch-map))

(defn not [x]
  (if x false true))

(defn not= [x & rest]
  (not (apply = x rest)))

(defmacro import [name]
  `(def ,name (require ,(str name))))

(defn update [x k f & rest]
  (let [oldv (get x k)
        newv (apply f oldv rest)]
    (put x k newv)))
