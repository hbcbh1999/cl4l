(defpackage cl4l-coro
  (:export cocall coresume coreturn)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-test))

(define-condition coreturn (condition)
  ((result :initarg :result :reader result)))

(defmacro coreturn (result)
  `(restart-case 
       (error 'coreturn :result ,result)
     (coresume ())))

(defmacro cocall ((var fn &rest args) &body body)
  (with-symbols (_c)
    `(handler-bind ((coreturn
                      #'(lambda (,_c)
                          (let ((,var (result ,_c)))
                            ,@body)
                          (invoke-restart 'coresume))))
       (funcall ,fn ,@args))))

(define-test (:coro)
  (flet ((foo (max)
           (dotimes (i max)
             (coreturn i))))
    (let ((sum 0))
      (cocall (i #'foo 10)
        (incf sum i))
      (assert (= 45 sum)))))
