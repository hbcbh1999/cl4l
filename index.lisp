(defpackage cl4l-index
  (:export index-add index-clone index-commit index-del index-diff
           index-find index-first
           index-join index-key index-last index-len
           index-match index-merge
           index-rem index-rollback
           make-index make-index-trans
           with-index-trans)
  (:shadowing-import-from cl4l-utils with-symbols)
  (:use cl cl4l-slist))

(in-package cl4l-index)

;; Default trans
(defvar *trans* nil)

(defmacro with-index-trans ((&key trans) &body body)
;; Executes BODY in transaction that is automatically
;; rolled back on early and committed on normal exit
  (with-symbols (_res)
    `(let ((*trans* (or ,trans (make-index-trans))))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (index-commit)
                ,_res))
         (index-rollback)))))

(defstruct (idx) 
  recs)

(defstruct (trans)
  add rem)

(defstruct (change)
  index key rec)

(defun make-index-trans ()
  (make-trans))

(defun make-index (&key key recs (uniq? t))
  ;; Returns a new index
  (let ((idx (make-idx :recs recs)))
    (unless (idx-recs idx)
            (setf (idx-recs idx)
                  (make-slist :key key :uniq? uniq?)))
    idx))

(defun index-find (self key &key rec start)
  ;; Returns item with KEY/IT in SELF, from START excl.;
  ;; or NIL if not found.
  (slist-find (idx-recs self) key :it rec :start start))

(defun index-add (self rec &key (key (index-key self rec))
                                start
                                (trans *trans*))
  ;; Returns items from KEY/REC in SELF
  (when (slist-add (idx-recs self) rec :key key :start start)
    (when trans
      (push (make-change :index self :key key :rec rec)
            (trans-add trans)))
    rec))

(defun index-clone (self)
  ;; Returns a clone of SELF
  (make-index :recs (slist-clone (idx-recs self))))

(defun trans-clear (self)
  (setf (trans-add self) nil
        (trans-rem self) nil))

(defun index-commit (&key (trans *trans*))
  ;; Clears changes made in TRANS
  (trans-clear trans))

(defun index-del (self prev)
  ;; Deletes item after PREV from SELF returns it
  (slist-del (idx-recs self) prev))

(defun index-diff (self other)
  ;; Removes all records from SELF that are found in OTHER and
  ;; returns SELF.
  (slist-diff (idx-recs self) (idx-recs other)))

(defun index-first (self &key key rec)
  ;; Returns all items in SELF, optionally from KEY/REC incl.
  (slist-first (idx-recs self) :key key :it rec))

(defun index-join (self other)
  ;; Removes all records from SELF that are not found in OTHER and
  ;; returns SELF.
  (slist-join (idx-recs self) (idx-recs other)))

(defun index-key (self rec)
  ;; Returns key for REC in SELF
  (slist-key (idx-recs self) rec))

(defun index-last (self)
  ;; Returns the last record from SELF
  (slist-last (idx-recs self)))

(defun index-rem (self key &key rec start (trans *trans*))
  ;; Removes KEY/REC from SELF after START and returns item
  (let ((rec (slist-rem (idx-recs self) key :it rec :start start)))
    (when (and rec trans)
      (push (make-change :index self :rec rec)
            (trans-rem trans)))
    rec))

(defun index-rollback (&key (trans *trans*))
  ;; Rolls back and clears changes made in TRANS
  (dolist (ch (nreverse (trans-add trans)))
    (index-rem (change-index ch) (change-key ch)
               :rec (change-rec ch)
               :trans nil))

  (dolist (ch (nreverse (trans-rem trans)))
    (index-add (change-index ch) (change-rec ch)
               :key (change-key ch)
               :trans nil))

  (trans-clear trans))

(defun index-len (self)
  ;; Returns length of SELF
  (slist-len (idx-recs self)))

(defun index-match (self other &key prev-match)
  ;; Returns next matching records from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH.  
  (slist-match (idx-recs self) (idx-recs other)
               :prev-match prev-match))

(defun index-merge (self other)
  ;; Adds all records from OTHER that are not found in SELF and
  ;; returns SELF.
  (slist-merge (idx-recs self) (idx-recs other)))
