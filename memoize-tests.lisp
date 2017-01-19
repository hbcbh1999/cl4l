(defpackage cl4l-memoize-tests
  (:use cl cl4l-memoize cl4l-test))

(in-package cl4l-memoize-tests)

(defparameter fib-max 25)

(define-test (:memoize :fib :naive)
  (labels ((fib (n)
             (case n
               (0 0)
               (1 1)
               (t (+ (fib (1- n)) (fib (- n 2)))))))
    (fib fib-max)))

(define-test (:memoize :fib)
  (with-memoize ()
    (labels ((fib (n)
               (do-memoize (:key n :pred (> n 10))
                 (case n
                   (0 0)
                   (1 1)
                   (t (+ (fib (1- n)) (fib (- n 2))))))))
      (fib fib-max))))

(define-test (:memoize :fn) ()
  (memoize-clear)
  (let* ((x 0)
         (fn (memoize (lambda (y) (incf x y)))))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 x))))
