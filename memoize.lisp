(defpackage cl4l-memoize
  (:export do-memoize make-memoize-context memoize memoize-clear
           with-memoize)
  (:shadowing-import-from cl4l-macro-utils with-gsyms)
  (:shadowing-import-from cl4l-test define-test)
  (:use cl))

(in-package cl4l-memoize)

(defun make-memoize-context ()
  ;; Returns new context
  (make-hash-table :test #'equal))

;; Default context
(defvar *context* (make-memoize-context))

(defmacro do-memoize ((cnd key &optional context)
                      &body body)
  ;; Memoizes BODY for ARGS
  (with-gsyms (_context _found _id _key)
    `(let* ((,_context (or ,context *context*))
            (,_key (list ',_id ,key))
            (,_found (gethash ,_key ,_context)))
       (or (and ,cnd ,_found)
           (setf (gethash ,_key ,_context)
                 (progn ,@body))))))

(defmacro with-memoize ((&optional context) &body body)
  ;; Executes BODY in context
  `(let (*context* (or ,context (make-memoize-context)))
     ,@body))

(defun memoize (fn)
  ;; Returns memoized wrapper for FN
  (lambda (&rest args)
    (do-memoize (t args)
      (apply fn args))))

(defun memoize-clear (&key (context *context*))
  ;; Clears CONTEXT
  (clrhash context))

;; Tests

(defparameter fib-max 25)

(define-test (:memoize :fib :perf :naive)
  (labels ((fib (n)
             (case n
               (0 0)
               (1 1)
               (t (+ (fib (1- n)) (fib (- n 2)))))))
    (fib fib-max)))

(define-test (:memoize :fib :perf)
  (memoize-clear)
  (labels ((fib (n)
             (do-memoize ((> n 10) n)
               (case n
                 (0 0)
                 (1 1)
                 (t (+ (fib (1- n)) (fib (- n 2))))))))
    (fib fib-max)))

(define-test (:memoize :fn) ()
  (memoize-clear)
  (let* ((x 0)
         (fn (memoize (lambda (y) (incf x y)))))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 (funcall fn 42)))
    (assert (= 42 x))))
