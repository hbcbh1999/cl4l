(defpackage cl4l-context
  (:export do-context make-context with-context)
  (:use common-lisp))

(in-package cl4l-context)

(defstruct (context (:conc-name cx-) (:constructor make-cx)))

(defun make-context (&rest args)
  ;; Returns a new context from ARGS
  (apply #'make-cx args))

(defmacro do-context (context &body body)
  ;; Executes BODY in CONTEXT
  `(let ((*default* ,context))
     ,@body))

(defmacro with-context ((&rest args) &body body)
  ;; Executes BODY in context from ARGS
  `(do-context (make-context ,@args)
     ,@body))
