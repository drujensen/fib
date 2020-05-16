(defn fib-iter [a b count]
  (if (= count -1)
    b
    (fib-iter (+ a b) a (- count 1))))

(defn fib [n]
  (fib-iter 1 0 n))

(pp (fib 46))
