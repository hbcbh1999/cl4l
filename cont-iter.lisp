(defpackage cl4l-cont-iter
  (:export iter-next iter-yield with-iter)
  (:shadowing-import-from cl4l-utils symbol! with-symbols)
  (:shadowing-import-from cl-cont lambda/cc)
  (:use cl cl4l-test))

(in-package cl4l-cont-iter)

(defmacro iter-yield (cont &optional result)
  (with-symbols (_cont)
    `(cl-cont:let/cc ,_cont
       (setf ,cont ,_cont)
       ,result)))

(defmacro with-iter ((cont expr) &body body)
  ;; Executes BODY with EXPR bound to optional NAME,
  ;; hard coded synonyms are provided for anonymous use.
  `(let ((iter-result ,expr))
     (macrolet ((iter-next ()
                  (let ((cn ',cont))
                    `(setf iter-result (funcall ,cn)))))
       ,@body)))

(defparameter test-max 100000)

(define-test (:cont-iter)
  (let* ((cont)
         (foo (lambda/cc (max)
                (dotimes (i max)
                  (iter-yield cont i))))
        (j 0))
    (with-iter (cont (funcall foo test-max))
        (assert (= j iter-result))
      (incf j)
      (iter-next))))

(define-test (:cont-iter :list)
  (flet ((foo (max)
           (let ((res))
             (dotimes (i max)
               (push i res))
             (nreverse res))))
    (let ((res (foo test-max)))
      (dotimes (j test-max)
        (assert (= (pop res) j))))))
