(defpackage cl4l-event
  (:export event-subscribe event-publish event-unsubscribe
           make-event)
  (:use cl))

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
  (mapcar (lambda (fn) (apply fn args)) (evt-subs self)))
