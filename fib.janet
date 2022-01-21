(defn fib [n]
  (if (>= 1 n)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))


(pp (fib 47))
