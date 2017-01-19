(defpackage cl4l-memoize
  (:export do-memoize make-memoize-context memoize memoize-clear
           with-memoize)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-test))

(in-package cl4l-memoize)

(defun make-memoize-context ()
  "Returns new context" 
  (make-hash-table :test #'equal))

(defvar *context* (make-memoize-context))

(defmacro do-memoize ((&key context key (pred t))
                      &body body)
  "Memoizes BODY for optional KEY if PRED" 
  (with-symbols (_pred _context _found? _id _key)
    `(let* ((,_pred ,pred)
            (,_context (or ,context *context*))
            (,_key (cons ',_id ,key))
            (,_found? (when ,_pred (gethash ,_key ,_context))))
       (if ,_pred
           (or ,_found? (setf (gethash ,_key ,_context)
                              (progn ,@body)))
           (progn ,@body)))))

(defmacro with-memoize ((&key context) &body body)
  "Executes BODY in context" 
  `(let ((*context* (or ,context
                        (make-memoize-context))))
     ,@body))

(defun memoize (fn)
  "Returns memoized wrapper for FN" 
  (lambda (&rest args)
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (do-memoize (:key args)
      (apply fn args))))

(defun memoize-clear (&key (context *context*))
  "Clears CONTEXT" 
  (clrhash context))
