(defpackage cl4l-event-tests
  (:use cl cl4l-event cl4l-test))

(in-package cl4l-event-tests)

(define-test (:event)
  (let ((evt (make-event)))
    (flet ((sub (x y) (> x y)))
      (event-subscribe evt #'sub)
      (assert (event-publish evt 2 1))
      (assert (null (event-publish evt 1 2))))))
