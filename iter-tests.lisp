(defpackage cl4l-iter-tests
  (:use cl cl4l-iter cl4l-test))

(in-package cl4l-iter-tests)

(defparameter test-max 100000)

(define-test (:iter :cond)
  (flet ((foo (max)
           (dotimes (i max)
             (iter-yield i))))
    (let ((j 0))
      (with-iter ((foo test-max))
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
