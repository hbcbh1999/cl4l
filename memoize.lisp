(defpackage cl4l-memoize
  (:export clear-memoized do-memoize memoize with-memoize
           memoize-tests)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use common-lisp))

(in-package cl4l-memoize)

(defun make-context ()
  ;; Returns new context
  (make-hash-table :test #'equal))

;; Default context
(defvar *context* (make-context))

(defmacro do-memoize ((args &key (context *context*))
                      &body body)
  ;; Memoizes BODY for ARGS in CONTEXT
  (with-gsyms (_args _context _found _id _key)
    `(let* ((,_context ,context)
            (,_args ,args)
            (,_key (cons ',_id ,_args))
            (,_found (gethash ,_key ,_context)))
       (or ,_found
           (setf (gethash ,_key ,_context)
                 (progn ,@body))))))

(defmacro with-memoize (&body body)
  ;; Executes BODY in new context
  `(do-context (make-context)
     ,@body))

(defun memoize (fn &key (context *context*))
  ;; Returns memoized wrapper for FN in CONTEXT
  (lambda (&rest args)
    (do-memoize (args :context context)
      (apply fn args))))

;; Tests

(defun memoize-tests ()
  (let* ((x 0)
         (fn (memoize (lambda (y) (incf x y)))))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 x))))
