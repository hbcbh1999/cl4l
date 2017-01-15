(defpackage cl4l-iter
  (:export iter-next iter-result iter-yield with-iter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:use cl cl4l-test))

(in-package cl4l-iter)

(defmacro iter-yield (&optional result)
  ;; Signals ITER-yield with RESULT
  `(restart-case 
       (signal 'iter-yield :result ,result)
     (iter-next ())))

(defmacro with-iter (name expr &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; ITER-aliases are provided for anonymous use.
  (let* ((_c (gensym))
         (_name (or name (gensym)))
         (_result (symbol! _name '-result)))
    `(macrolet ((,(symbol! _name '-next) ()
                  `(invoke-restart 'iter-next))
                (iter-next ()
                  `(,(symbol! ',_name '-next)))
                (,_result () `(result ,',_c))
                (iter-result () `(,',_result)))
       (handler-bind ((iter-yield
                        (lambda (,_c)
                          (declare (ignorable ,_c))
                          ,@body)))
         ,expr))))

(define-condition iter-yield (condition)
  ((result :initarg :result :reader result)))

(defparameter test-max 100000)

(define-test (:iter :cond)
  (flet ((foo (max)
           (dotimes (i max)
             (iter-yield i))))
    (let ((j 0))
      (with-iter nil (foo test-max)
        (assert (= j (iter-result)))
        (incf j)
        (iter-next)))))

(define-test (:iter :list)
  (flet ((foo (max)
           (let ((res))
             (dotimes (i max)
               (push i res))
             (nreverse res))))
    (let ((res (foo test-max)))
      (dotimes (j test-max)
        (assert (= (pop res) j))))))
