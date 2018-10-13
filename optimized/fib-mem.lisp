
(declaim (optimize (speed 3) (debug 0) (safety 0)))
(declaim (ftype    (function (fixnum) fixnum) fib))

(let ((cache (make-array 100 :element-type 'fixnum :initial-element -1)))
  (declare ((simple-array fixnum (100)) cache))
  (defun fib (n)
    (if (<= n 1)
        (setf (aref cache n) 1)
        (let ((cached (aref cache n)))
          (if (plusp cached)
              cached
              (setf (aref cache n)
                    (the fixnum
                         (+ (fib (- n 2))
                            (fib (1- n))))))))))

(defun main ()
  (write (fib 46)))

#+sbcl
(sb-ext:save-lisp-and-die "fib" :toplevel #'main :executable t)

#+ccl
(ccl:save-application "fib" :toplevel-function #'main :prepend-kernel t)
