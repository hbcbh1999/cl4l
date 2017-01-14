(defpackage cl4l-coro
  (:export do-coro coro-resume coro-return)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-test))

(in-package cl4l-coro)

(defmacro coro-return (&optional result)
  ;; Signals CORO-RETURN with RESULT
  `(restart-case 
       (signal 'coro-return :result ,result)
     (coro-resume ())))

(defmacro do-coro ((var expr) &body body)
  ;; Executes BODY for every CORO-RETURN
  ;; caused by evaluating EXPR,
  ;; with VAR bound to the returned result
  (with-symbols (_c)
    `(handler-bind ((coro-return
                      (lambda (,_c)
                        (let ((,var (result ,_c)))
                          ,@body)
                        (invoke-restart 'coro-resume))))
       ,expr)))

(define-condition coro-return (condition)
  ((result :initarg :result :reader result)))

(define-test (:coro)
  (flet ((foo (max)
           (dotimes (i max)
             (coro-return i))))
    (let ((sum 0))
      (do-coro (i (foo 10))
        (incf sum i))
      (assert (= 45 sum)))))
