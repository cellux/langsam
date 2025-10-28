(def args nil)

(defn nextarg []
  (when-let [[arg & rest] args]
    (setq args rest)
    arg))
