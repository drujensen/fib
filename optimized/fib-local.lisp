
#|

compile (sbcl):   sbcl --load fib-local.lisp
compile (ccl):    ccl  --load fib-local.lisp


run:

time ./fib-local

|#

#+sbcl
(progn
  ;; In C, compiler optimizes the code...
  ;; In Soviet Russia, the code optimizes the compiler
  ;; 
  ;; (setf sb-ext:*inline-expansion-limit* 2) ; 0m14.745s
  ;; (setf sb-ext:*inline-expansion-limit* 3) ; 0m9.728s
  (setf sb-ext:*inline-expansion-limit* 4) ; 0m7.693s
  ;; (setf sb-ext:*inline-expansion-limit* 5) ; 0m10.770s
  ;; (setf sb-ext:*inline-expansion-limit* 6) ; 0m11.244s
  ;; (setf sb-ext:*inline-expansion-limit* 7) ; 0m10.870s

  ;; Note: If you see the assembly code of GCC, you see that
  ;; GCC is doing the similar inlining optimization
  )

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(declaim (ftype    (function (fixnum) fixnum) fib))
(defun fib (n)
  (labels ((rec (n)
             (if (<= n 1)
                 1
                 (+ (rec (1- n))
                    (rec (- n 2))))))
    (declare (ftype (function (fixnum) fixnum) rec))
    (declare (inline rec))
    (rec n)))

(defun main ()
  (write (fib 46)))

#+sbcl
(sb-ext:save-lisp-and-die "fib-local" :toplevel #'main :executable t)

#+ccl
(ccl:save-application "fib-local" :toplevel-function #'main :prepend-kernel t)

