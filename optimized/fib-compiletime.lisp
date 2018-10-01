(declaim (optimize (speed 3) (debug 0) (safety 0)))
(declaim (ftype    (function (fixnum) fixnum) fib))
(defun fib (n)
  (if (<= n 1)
      1
      (+ (fib (1- n))
         (fib (- n 2)))))

(defun main ()
  ;; This computes (fib 46) in compile time
  ;; In lisp, ANYTHING can be done in compile time, therefore
  ;; you don't need an overly complicated constexpr!
  (write #.(fib 46)))

#+sbcl
(sb-ext:save-lisp-and-die "fib-compiletime" :toplevel #'main :executable t)

#+ccl
(ccl:save-application "fib-compiletime" :toplevel-function #'main :prepend-kernel t)

