(defpackage cl4l-test
  (:export define-test test run-tests)
  (:import-from cl4l-macro-utils with-gsyms)
  (:import-from cl4l-utils do-bench)
  (:use cl cl4l-slist))

(in-package cl4l-test)

(defparameter *suite* nil)

(defmacro define-test ((&rest tags) &body body)
  `(test (lambda () ,@body) '(,@tags)))

(defun test (fn &rest tags)
  (push (cons (apply #'slist nil tags) fn)
        *suite*))

(defun run-tests (nprep nreal &optional run-tags skip-tags)
  (dolist (tst *suite*)
    (let ((test-tags (first tst)))
      (when (and (or (null run-tags)
                     (slist-match test-tags run-tags))
                 (or (null skip-tags)
                     (not (slist-match test-tags skip-tags))))
        (do-bench (nprep nreal)
          (funcall (rest tst)))))))

(define-test (foo bar)
  (format t "testing"))
