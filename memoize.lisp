(defpackage cl4l-memoize
  (:export do-memoize memoize with-memoize
           memoize-tests)
  (:use common-lisp))

(in-package cl4l-memoize)

(defun make-context ()
  ;; Returns new context
  (make-hash-table :test #'equal))

(defparameter *context* (make-context))

(defmacro do-memoize ((args &key (context *context*)) &body body)
  ;; Memoizes BODY for ARGS in CONTEXT
  (let ((_args (gensym))
        (_context (gensym))
        (_found (gensym))
        (_id (gensym))
        (_key (gensym)))
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
  ;; Returns memoized wrapper for FN
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
