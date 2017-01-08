(defpackage cl4l-memoize
  (:export make-memoize
           do-memoize memoize memoize-clear with-memoize)
  (:import-from cl4l-macro-utils with-gsyms)
  (:import-from cl4l-test define-test)
  (:use cl))

(in-package cl4l-memoize)

(defun make-memoize ()
  ;; Returns new context
  (make-hash-table :test #'equal))

;; Default context
(defvar *context* (make-memoize))

(defmacro do-memoize ((cnd key) &body body)
  ;; Memoizes BODY for ARGS
  (with-gsyms (_found _id _key)
    `(let* ((,_key (list ',_id ,key))
            (,_found (gethash ,_key *context*)))
       (or (and ,cnd ,_found)
           (setf (gethash ,_key *context*)
                 (progn ,@body))))))

(defmacro with-memoize (&body body)
  ;; Executes BODY in new context
  `(let (*context* (make-memoize))
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
