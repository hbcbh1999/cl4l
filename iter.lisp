(defpackage cl4l-iter
  (:export iter-next iter-yield with-iter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:use cl cl4l-test))

(in-package cl4l-iter)

(defmacro iter-yield (&optional result)
  ;; Signals ITER-yield with RESULT
  `(restart-case 
       (signal 'iter-yield :result ,result)
     (iter-next ())))

(defmacro with-iter (name expr &body body)
  (with-symbols (_c)
    (let* ((_name (or name (gensym)))
           (_result (symbol! _name '-result)))
      `(macrolet ((,(symbol! _name '-next) ()
                    `(invoke-restart 'iter-next))
                  (iter-next ()
                    `(,(symbol! ',_name '-next))))
         (handler-bind ((iter-yield
                          (lambda (,_c)
                            (let* ((,_result (result ,_c))
                                  (iter-result ,_result))
                              (declare (ignorable ,_result
                                                  iter-result))
                              ,@body))))
           ,expr)))))

(define-condition iter-yield (condition)
  ((result :initarg :result :reader result)))

(define-test (:iter)
  (flet ((foo (max)
           (dotimes (i max)
             (iter-yield i))))
    (let ((sum 0))
      (with-iter nil (foo 10)
        (incf sum iter-result)
        (iter-next))
      (assert (= 45 sum)))))
