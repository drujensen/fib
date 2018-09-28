(defn fib 
  ([n] 
   (fib [0 1] n))
  ([x, n]
   (if (< (count x) n)
     (fib (conj x (+ (last x) (nth x (- (count x) 2)))) n)
     x)))
(println (fib 46))
