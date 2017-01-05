(defpackage cl4l-memoize
  (:export define-memoized do-memoize memoize memoize-tests
           with-memoize)
  (:use common-lisp))

(in-package cl4l-memoize)

(defstruct (mz)
  fn key res)

(defmacro define-memoized (sym)
  `(progn
     (let ((fn (symbol-function ',sym)))
       (fmakunbound ',sym)
       (setf (symbol-function ',sym) (memoize fn)))))

(defmacro do-memoize ((context) &body body)
  ;; Executes BODY in CONTEXT
  `(let ((*context* ,context))
     ,@body))

(defmacro with-memoize (&body body)
  ;; Executes BODY in context from ARGS
  `(do-context (make-context)
     ,@body))

(defun make-context ()
  (make-hash-table :test #'equal))

(defparameter *context* (make-context))

(defun memoize (fn &key (context *context*))
  ;; Returns memoized wrapper for FN
  (let ((id (gensym)))
    (lambda (&rest args)
      (let* ((key (cons id args))
             (found (gethash key context))
             (mz (or found
                     (setf (gethash key context)
                           (make-mz :fn fn
                                    :res (apply fn args))))))
        (mz-res mz)))))

(let ((x 0))
  (defun test-fn (y)
    (incf x y)))

(define-memoized test-fn)

(defun basic-tests ()
  (let* ((x 0)
         (fn (memoize (lambda (y) (incf x y)))))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 x))))

(defun define-tests ()
  (assert (= 42 (funcall #'test-fn 42)))
  (assert (= 42 (funcall #'test-fn 42))))

(defun memoize-tests ()
  (basic-tests)
  (define-tests))
