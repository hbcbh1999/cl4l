(defpackage cl4l-test
  (:export define-test run-tests test untest)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use cl cl4l-slist))

(in-package cl4l-test)

(defvar *suite* (slist #'first))

(defmacro define-test ((&rest tags) &body body)
  `(test (list ,@tags) (lambda () ,@body)))

(defmacro do-test ((warmup reps) &body body)
  (with-gsyms (_real _run)
    `(progn
       (dotimes (_ ,warmup) ,@body)
       (let ((,_real (get-internal-real-time))
             (,_run (get-internal-run-time)))
         (dotimes (_ ,reps) ,@body)
         (cons (/ (- (get-internal-real-time) ,_real)
                  internal-time-units-per-second)
               (/ (- (get-internal-run-time) ,_run)
                  internal-time-units-per-second))))))

(defun test (tags fn &key (suite *suite*))
  (let ((found (slist-find suite tags nil)))
    (if found
        (setf (rest found) fn)
        (slist-add suite (cons tags fn)))))

(defun untest (tags &key (suite *suite*))
  (slist-rem suite tags nil))

(defgeneric run-tests (tags &key)  
  (:method (tags &key (warmup 0) (reps 1)
                      skip
                      (suite *suite*))

    (let ((tot-time 0))
      (dolist (test (slist-first suite))
        (let ((test-tags (first test))
              (test-fn (rest test)))
          (when (and (or (null tags)
                         (intersection test-tags tags))
                     (or (null skip)
                         (not (intersection test-tags skip))))
            (format t "testing ~30a" test-tags)
            (dotimes (_ warmup) (funcall test-fn))
            (let ((time (get-internal-run-time)))
              (dotimes (_ reps) (funcall test-fn))
              (setf time (- (get-internal-run-time) time))
              (format t "~5f~%"
                      (/ time internal-time-units-per-second))
              (incf tot-time time)))))
      (format t "TOTAL ~32a~5f~%" ""
              (/ tot-time internal-time-units-per-second)))))
