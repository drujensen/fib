(defn fib [n]
  (if (<= n 1) 1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(println (fib 46))
