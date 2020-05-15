(def results @{})

(defn fib-mem [n]
  (if (get results n)
    (get results n)
    (do
      (def im
        (if (>= 1 n)
          1
          (+ (fib-mem (- n 1))
             (fib-mem (- n 2)))))
      (put results n im)
      im)))

(pp (fib-mem 46))
