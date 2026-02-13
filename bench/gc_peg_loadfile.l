(import os)

(let [runs 10]
  (if (<= runs 0)
    (raise 'gc-stress "run count must be positive, got %s" runs))
  (os/write os/stdout
            (format "GC stress: loading tests/slow/peg.l %s times\n" runs))
  (gc)
  (for [i (range 1 (+ runs 1))]
    (let [env (sublet (curlet))
          load-result (os/loadfile env "tests/slow/peg.l")
          stats (gc)]
      (assert (not (= (type load-result) Exception)))
      (os/write os/stdout
                (format "run %s/%s swept=%s marked=%s\n"
                        i
                        runs
                        (:swept stats)
                        (:marked stats)))))
  (let [final (gc)]
    (os/write os/stdout
              (format "final swept=%s marked=%s\n"
                      (:swept final)
                      (:marked final)))))
