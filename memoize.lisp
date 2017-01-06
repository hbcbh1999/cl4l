(defpackage cl4l-memoize
  (:export make-context
           do-memoize memoize memoize-clear with-memoize
           memoize-tests)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use common-lisp))

(in-package cl4l-memoize)

(defun make-context ()
  ;; Returns new context
  (make-hash-table :test #'equal))

;; Default context
(defvar *context* (make-context))

(defmacro do-memoize ((args) &body body)
  ;; Memoizes BODY for ARGS
  (with-gsyms (_args _found _id _key)
    `(let* ((,_args ,args)
            (,_key (cons ',_id ,_args))
            (,_found (gethash ,_key *context*)))
       (or ,_found
           (setf (gethash ,_key *context*)
                 (progn ,@body))))))

(defmacro with-memoize (&body body)
  ;; Executes BODY in new context
  `(let (*context* (make-context))
     ,@body))

(defun memoize (fn)
  ;; Returns memoized wrapper for FN
  (lambda (&rest args)
    (do-memoize (args)
      (apply fn args))))

(defun memoize-clear ()
  (clrhash *context*))

;; Tests

(defun fib-tests ()
  (labels ((fib (n)
             (do-memoize ((list n))
               (case n
                 (0 0)
                 (1 1)
                 (t (+ (fib (1- n)) (fib (- n 2))))))))
    (fib 10000)))

(defun fn-tests ()
  (let* ((x 0)
         (fn (memoize (lambda (y) (incf x y)))))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 x))))

(defun memoize-tests ()
  (memoize-clear)
  (fib-tests)
  (fn-tests))
