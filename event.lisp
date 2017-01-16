(defpackage cl4l-event
  (:export event-subscribe event-publish event-unsubscribe
           make-event)
  (:use cl))

(in-package cl4l-event)

(defstruct (event (:conc-name ev-) (:constructor make-ev))
  subs)

(defun make-event (&rest args)
  (apply #'make-ev args))

(defun event-subscribe (self fn)
  (setf (ev-subs self) (adjoin fn (ev-subs self))))

(defun event-unsubscribe (self fn)
  (setf (ev-subs self) (remove fn (ev-subs self))))

(defun event-publish (self &rest args)
  (mapcar (lambda (fn) (apply fn args)) (ev-subs self)))
