(defpackage cl4l-context
  (:export context-memoize do-context make-context with-context)
  (:use cl4l-slist common-lisp ))

(in-package cl4l-context)

(defstruct (context (:conc-name cx-) (:constructor make-cx))
  (memoized (make-slist :key #'mz-key)))

(defvar *default* nil)

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

(defstruct (memoized (:conc-name mz-) (:constructor make-mz))
  key res)

(defun context-memoize (fn args &key (cx *default*))
  (let* ((mzd (cx-memoized cx))
        (key (cons fn args))
         (found (slist-find mzd key))
         (mz (or found
                 (slist-add mzd (make-mz :key key
                                         :res (apply fn args))))))
    (mz-res mz)))
