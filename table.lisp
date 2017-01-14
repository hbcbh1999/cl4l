(defpackage cl4l-table
  (:export clone-record
           make-table make-table-trans
           table-delete table-find table-stored? table-upsert
           with-table-trans)
  (:shadowing-import-from cl4l-utils compare with-symbols)
  (:use cl cl4l-index))

(in-package cl4l-table)

(defstruct (tbl)
  recs
  (stored (make-hash-table :test #'eq)))

(defun make-table (key)
  (make-tbl :recs (index key)))

(defun table-key (self rec)
  (index-key (tbl-recs self) rec))

(defun table-find (self key)
  ;; Returns record with KEY from SELF,
  ;; or NIL if not found.
  (index-find (tbl-recs self) key))
  
(defun table-upsert (self rec &key (key (table-key self rec)))
  (multiple-value-bind (prev found?)
      (index-prev (tbl-recs self) key)
    (if found?
        (rplaca (rest prev) rec)
        (index-insert (tbl-recs self) prev rec))
    (setf (gethash self (tbl-stored self))
          (clone-record rec))))

(defun table-delete (self rec)
  (index-remove (tbl-recs self) (table-key self rec))
  (remhash rec (tbl-stored self)))

(defun table-stored? (self rec)
  (gethash rec (tbl-stored self)))

(defgeneric clone-record (self))
