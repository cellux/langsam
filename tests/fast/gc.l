(let [stats (gc)]
  (assert (integer? (:marked stats)))
  (assert (integer? (:swept stats)))
  (assert (< (:marked stats) 1000000000000))
  (assert (< (:swept stats) 1000000000000)))
