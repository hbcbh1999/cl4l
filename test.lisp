(defpackage cl4l-test
  (:export define-test run-suite run-test test untest)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-index))

(in-package cl4l-test)

(defvar *suite* (index #'first))

(defmacro define-test ((&rest tags) &body body)
  `(test (list ,@tags) (lambda () ,@body)))

(defun test (tags fn &key (suite *suite*))
  (let ((found? (index-find suite tags)))
    (if found?
        (rplacd found? fn)
        (index-add suite (cons tags fn)))))

(defun untest (tags &key (suite *suite*))
  (index-remove suite tags))

(defgeneric run-test (tags fn &key warmup reps)
  (:method (tags fn &key (warmup 0) (reps 1))
    (format t "testing ~30a" tags)
    (dotimes (_ warmup) (funcall fn))
    (let ((time (get-internal-run-time)))
      (dotimes (_ reps) (funcall fn))
      (setf time (- (get-internal-run-time) time))
      (format t "~5f~%"
              (/ time internal-time-units-per-second))
      time)))

(defgeneric run-suite (tags &key)  
  (:method (tags &key (warmup 0) (reps 1)
                      skip
                      (suite *suite*))
    (tagbody
     retry-suite
       (let ((tot-time 0))
         (dolist (test (index-first suite))
           (let ((test-tags (first test))
                 (test-fn (rest test)))
             (when (and (or (null tags)
                            (null (set-difference tags test-tags)))
                        (or (null skip)
                            (not (intersection test-tags skip))))
               (tagbody
                retry-test
                  (restart-case 
                      (incf tot-time
                            (run-test test-tags test-fn
                                      :warmup warmup
                                      :reps reps))
                    (skip-test ()
                      (format t " SKIP~%"))
                    (retry-test ()
                      (format t " TEST~%")
                      (go retry-test))
                    (retry-suite ()
                      (format t "SUITE~%")
                      (go retry-suite)))))))
         (format t "TOTAL ~32a~5f~%" ""
                 (/ tot-time internal-time-units-per-second))))))
