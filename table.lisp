(defpackage cl4l-table
  (:export clone-record
           make-table make-table-trans
           table table-delete table-find table-key table-length
           table-prev? table-upsert
           with-table-trans)
  (:shadowing-import-from cl4l-utils compare key-gen with-symbols)
  (:use cl))

(in-package cl4l-table)

(defstruct (tbl)
  key-gen
  recs
  (prev (make-hash-table :test #'eq)))

(defun make-table (&key key key-gen (test #'equal))
  (make-tbl :key-gen (or key-gen (key-gen key))
            :recs (make-hash-table :test test)))

(defun table (key &rest recs)
  ;; Returns a new table with KEY from RECS
  (let ((tbl (make-table :key key)))
    (dolist (rec recs tbl)
      (table-upsert tbl rec))))

(defun table-find (self key)
  ;; Returns record with KEY from SELF,
  ;; or NIL if not found.
  (gethash key (tbl-recs self)))

(defun table-key (self rec)
  (funcall (tbl-key-gen self) rec))

(defun table-length (self)
  (hash-table-count (tbl-recs self)))

(defun table-upsert (self rec &key (key (table-key self rec)))
  (setf (gethash key (tbl-recs self)) rec))

(defun table-delete (self rec)
  (remhash (table-key self rec) (tbl-recs self))
  (remhash rec (tbl-prev self)))

(defun table-prev? (self rec)
  (gethash rec (tbl-prev self)))

(defgeneric clone-record (self))
