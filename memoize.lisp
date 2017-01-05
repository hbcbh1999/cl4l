(defpackage cl4l-memoize
  (:export defmoize memoize)
  (:use cl4l-context common-lisp))

(in-package cl4l-memoize)

(defun memoize (fn)
  ;; Returns memoized wrapper for FN
  (lambda (&rest args)
    (context-memoize fn args)))
