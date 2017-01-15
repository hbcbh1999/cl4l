(defpackage cl4l-event-tests
  (:use cl cl4l-event cl4l-test))

(in-package cl4l-event-tests)

(define-test (:event)
  (let ((evt (make-event)))
    (flet ((fn (x y) (> x y)))
      (event-subscribe evt #'fn)
      (assert (equal (event-publish evt 2 1) '(t)))
      (assert (equal (event-publish evt 1 2) '(nil))))))
