(defpackage cl4l-table
  (:export clone-record
           make-table make-table-trans
           table table-commit table-delete table-find
           table-index table-key
           table-length table-prev? table-rollback table-upsert
           with-table-trans *table-trans*)
  (:shadowing-import-from cl4l-utils compare key-gen with-symbols)
  (:use cl cl4l-index))

(in-package cl4l-table)

;; Default trans
(defvar *table-trans* nil)

(defmacro with-table-trans ((&key trans) &body body)
  ;; Executes BODY in transaction that is automatically
  ;; rolled back on early and committed on normal exit
  (with-symbols (_res)
    `(let ((*table-trans* (or ,trans (make-table-trans))))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (table-commit)
                ,_res))
         (table-rollback)))))

(defstruct (tbl)
  idxs
  key-gen
  recs
  (prev (make-hash-table :test #'eq)))

(defstruct (ch)
  op tbl rec prev)

(defun make-table (&key key key-gen (test #'equal))
  (make-tbl :key-gen (or key-gen (key-gen key))
            :recs (make-hash-table :test test)))

(defun table (key &rest recs)
  ;; Returns a new table with KEY from RECS
  (let ((tbl (make-table :key key)))
    (dolist (rec recs tbl)
      (table-upsert tbl rec))))

(defun make-table-trans ()
  (list nil))

(defun table-trans-reset (self)
  (rplacd self nil))

(defun table-commit (&key (trans *table-trans*))
  ;; Clears changes made in TRANS
  (when trans
    (table-trans-reset trans)))

(defun table-index (self idx)
  (setf (tbl-idxs self) (adjoin idx (tbl-idxs self)))
  idx)

(defun table-find (self key)
  ;; Returns record with KEY from SELF,
  ;; or NIL if not found.
  (gethash key (tbl-recs self)))

(defun table-key (self rec)
  (funcall (tbl-key-gen self) rec))

(defun table-length (self)
  (hash-table-count (tbl-recs self)))

(defun table-upsert (self rec &key (trans *table-trans*))
  (let ((key (table-key self rec)))
    (when trans
      (push (make-ch :op 'upsert
                     :tbl self
                     :rec rec
                     :prev (gethash key (tbl-recs self)))
            (rest  trans)))
    (when (tbl-idxs self)
      (let ((prev (gethash rec (tbl-prev self))))
        (dolist (idx (tbl-idxs self))
          (when prev
            (index-remove idx (index-key idx prev)))
          (index-add idx rec))))
  (setf (gethash key (tbl-recs self)) rec)
  (setf (gethash rec (tbl-prev self)) (clone-record rec))))

(defun table-delete (self rec &key (trans *table-trans*))
  (let ((prev (gethash rec (tbl-prev self))))
    (when prev
      (let ((key (table-key self rec)))
        (when trans
          (push (make-ch :op 'delete
                         :tbl self
                         :rec rec
                         :prev (gethash key (tbl-recs self)))
                (rest trans)))
        
        (when (tbl-idxs self)
          (let ()
            (when prev
              (dolist (idx (tbl-idxs self))
                (index-remove idx (index-key idx prev))))))

        (remhash key (tbl-recs self))
        (remhash rec (tbl-prev self))))))

(defun table-prev? (self rec)
  (gethash rec (tbl-prev self)))

(defun table-rollback (&key (trans *table-trans*))
  ;; Rolls back and clears changes made in TRANS
  (when trans
    (dolist (ch (nreverse (rest trans)))
      (ecase (first trans)
        (upsert
         (if (ch-prev ch)
             (table-upsert (ch-tbl ch) (ch-prev ch) :trans nil)
             (table-delete (ch-tbl ch) (ch-rec ch) :trans nil)))
        (delete
         (table-upsert (ch-tbl ch) (ch-rec ch) :trans nil))))

    (table-trans-reset trans)))

(defgeneric clone-record (self))
