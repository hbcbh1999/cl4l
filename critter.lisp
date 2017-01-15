(defpackage cl4l-critter
  (:export critter-next critter-yield do-critter with-critter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:shadowing-import-from cl-cont lambda/cc)
  (:use cl cl4l-test))

(in-package cl4l-critter)

(defmacro critter-yield (cont &optional result)
  (with-symbols (_cont)
    `(cl-cont:let/cc ,_cont
       (setf ,cont ,_cont)
       ,result)))

(defmacro do-critter ((cont expr) &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; hard coded synonyms are provided for anonymous use.
  `(let ((critter-result ,expr))
     (macrolet ((critter-next ()
                  (let ((cn ',cont))
                    `(setf critter-result (funcall ,cn)))))
       ,@body)))

(defparameter test-max 100000)

;; todo add with-critter macro to encapsulate lambda/cc && cont

(define-test (:critter)
  (let* ((cont)
         (foo (lambda/cc (max)
                (dotimes (i max)
                  (critter-yield cont i))))
         (j 0))
    (do-critter (cont (funcall foo test-max))
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
