(defpackage cl4l-iter
  (:export do-iter iter-next iter-result iter-yield with-iter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:use cl))

(in-package cl4l-iter)

(defmacro do-iter ((expr it) &body body)
  ;; Iterates body with IT bound to items from EXPR
  `(with-iter (,expr)
     (let ((,it (iter-result)))
       ,@body)
     (iter-next)))

(defmacro iter-yield (&optional result)
  ;; Signals ITER-yield with RESULT
  `(restart-case 
       (signal 'iter-yield :result ,result)
     (iter-next ())))

(defmacro with-iter ((expr &key name) &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; aliases are provided for anonymous use.
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
