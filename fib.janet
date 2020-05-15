(defn fib [n]
  (if (>= 1 n)
    1
    (+ (fib (- n 1))
       (fib (- n 2)))))


(pp (fib 46))
