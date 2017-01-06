(defpackage cl4l-utils
  (:export do-hash-values)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use common-lisp))

(in-package cl4l-utils)

(defmacro do-hash-values ((tbl val) &body body)
  (with-gsyms (_found _iter _key)
    `(with-hash-table-iterator (,_iter ,tbl)
       (tagbody
        start
          (multiple-value-bind (,_found ,_key ,val) (,_iter)
            (declare (ignore ,_key))
            (unless ,_found (go end))
            ,@body)
          (go start)
        end))))
