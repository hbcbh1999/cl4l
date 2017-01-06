(defpackage cl4l-utils
  (:export do-bench do-hash-table)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use common-lisp))

(in-package cl4l-utils)

(defmacro do-bench ((num-warmups num-reps) &body body)
  `(progn
     (dotimes (_ ,num-warmups) ,@body)
     (time
      (dotimes (_ ,num-reps) ,@body))))

(defmacro do-hash-table ((tbl key val) &body body)
  (with-gsyms (_found _iter)
    `(with-hash-table-iterator (,_iter ,tbl)
       (tagbody
        start
          (multiple-value-bind (,_found ,key ,val) (,_iter)
            (declare (ignorable ,key ,val))
            (unless ,_found (go end))
            ,@body)
          (go start)
        end))))
