#?(:clj (set! *unchecked-math* true))

(defn fib ^long [^long n]
  (if (<= n 1) n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(println (fib 47))
