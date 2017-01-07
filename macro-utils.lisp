(defpackage cl4l-macro-utils
  (:export with-syms)
  (:use cl))

(in-package cl4l-macro-utils)

(defmacro with-gsyms ((&rest vars) &body body)
  `(let (,@(mapcar (lambda (v) `(,v (gensym))) vars))
     ,@body))
