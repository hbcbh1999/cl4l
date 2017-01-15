(defpackage cl4l-event
  (:export event-subscribe event-publish event-unsubscribe)
  (:use cl cl4l-test))

(in-package cl4l-event)

(defstruct (evt)
  subs)

(defun make-event (&rest args)
  (apply #'make-evt args))

(defun event-subscribe (self fn)
  (setf (evt-subs self) (adjoin fn (evt-subs self))))

(defun event-unsubscribe (self fn)
  (setf (evt-subs self) (remove fn (evt-subs self))))

(defun event-publish (self &rest args)
  (dolist (fn (evt-subs self) t)
    (unless (apply fn args)
      (return nil))))

(define-test (:event)
  (let ((evt (make-event)))
    (flet ((sub (x y) (> x y)))
      (event-subscribe evt #'sub)
      (assert (event-trigger evt 2 1))
      (assert (null (event-trigger evt 1 2))))))
