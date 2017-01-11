(defpackage cl4l-coro
  (:export coro-do coro-resume coro-return)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-test))

(in-package cl4l-coro)

(define-condition coro-return (condition)
  ((result :initarg :result :reader result)))

(defmacro coro-return (result)
  `(restart-case 
       (error 'coro-return :result ,result)
     (coro-resume ())))

(defmacro coro-do ((var expr) &body body)
  (with-symbols (_c)
    `(handler-bind ((coro-return
                      (lambda (,_c)
                        (let ((,var (result ,_c)))
                          ,@body)
                        (invoke-restart 'coro-resume))))
       ,expr)))

(define-test (:coro)
  (flet ((foo (max)
           (dotimes (i max)
             (coro-return i))))
    (let ((sum 0))
      (coro-do (i (foo 10))
        (incf sum i))
      (assert (= 45 sum)))))
