(defpackage cl4l-critter
  (:export critter critter-next critter-yield with-critter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:shadowing-import-from cl-cont lambda/cc)
  (:use cl cl4l-test))

(in-package cl4l-critter)

(defmacro critter ((context args) &body body)
  `(lambda/cc ,args
     (macrolet ((critter-yield (&optional result)
                  (let ((_context ',context))
                    `(cl-cont:let/cc c
                       (setf ,_context c)
                       ,result))))
       ,@body)))

(defmacro critter-yield (context &optional result)
  (with-symbols (_context)
    `(cl-cont:let/cc ,_context
       (setf ,context ,_context)
       ,result)))

(defmacro with-critter ((context expr) &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; hard coded synonyms are provided for anonymous use.
  `(let ((critter-result ,expr))
     (macrolet ((critter-next ()
                  (let ((_context ',context))
                    `(setf critter-result (funcall ,_context)))))
       ,@body)))
  
(defparameter test-max 100000)

(define-test (:critter)
  (let* ((context)
         (foo (critter (context (max))
                (dotimes (i max)
                  (critter-yield i))))
         (j 0))
    (with-critter (context (funcall foo test-max))
      (assert (= j critter-result))
      (incf j)
      (critter-next))))

(define-test (:critter :list)
  (flet ((foo (max)
           (let ((res))
             (dotimes (i max)
               (push i res))
             (nreverse res))))
    (let ((res (foo test-max)))
      (dotimes (j test-max)
        (assert (= (pop res) j))))))
