(defpackage cl4l-index
  (:export index-add index-clone index-commit index-del index-diff
           index-find index-first
           index-join index-key index-last index-len
           index-match index-merge
           index-rem index-rollback
           make-index make-trans
           with-index)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use cl cl4l-slist))

(in-package cl4l-index)

;; Default trans
(defvar *trans* nil)

(defmacro with-index (&body body)
;; Executes BODY in new transaction
  (with-gsyms (_res)
    `(let ((*trans* (make-trans)))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (index-commit)
                ,_res))
         (index-rollback)))))

(defstruct (idx) 
  keys name recs)

(defstruct (trans)
  add rem)

(defstruct (change)
  index key rec)

(defun index-key (self rec)
  ;; Returns key for REC in SELF
  (mapcar (lambda (fn)
            (if (eq fn t) rec (funcall fn rec)))
          (idx-keys self)))

(defun make-index (keys &key (name (gensym)) recs uniq?)
  ;; Returns a new index
  (let ((idx (make-idx :keys keys
                       :name name
                       :recs recs)))
    (unless (idx-recs idx)
            (setf (idx-recs idx)
                  (make-slist :key (lambda (rec)
                                     (index-key idx rec))
                              :uniq? uniq?)))
    idx))

(defun index-find (self key &optional rec)
  ;; Returns items from KEY/REC in SELF
  (slist-find (idx-recs self) key rec))

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
  (make-index (idx-keys self)
              :recs (slist-clone (idx-recs self))))

(defun trans-clear (self)
  (setf (trans-add self) nil
        (trans-rem self) nil))

(defun index-commit (&optional (trans *trans*))
  ;; Clears changes made in TRANS
  (trans-clear trans))

(defun index-del (self prev)
  ;; Deletes item after PREV from SELF returns it
  (slist-del (idx-recs self) prev))

(defun index-diff (self other)
  ;; Removes all records from SELF that are found in OTHER and
  ;; returns SELF.
  (slist-diff (idx-recs self) (idx-recs other)))

(defun index-first (self &optional key rec)
  ;; Returns all items in SELF, optionally from KEY/REC incl.
  (slist-first (idx-recs self) key rec))

(defun index-join (self other)
  ;; Removes all records from SELF that are not found in OTHER and
  ;; returns SELF.
  (slist-join (idx-recs self) (idx-recs other)))

(defun index-last (self)
  ;; Returns the last record from SELF
  (slist-last (idx-recs self)))

(defun index-rem (self key rec &key start (trans *trans*))
  ;; Removes KEY/REC from SELF after START and returns item
  (let ((rec (slist-rem (idx-recs self) key rec :start start)))
    (when (and rec trans)
      (push (make-change :index self :rec rec)
            (trans-rem trans)))
    rec))

(defun index-rollback (&optional (trans *trans*))
  ;; Rolls back and clears changes made in TRANS
  (dolist (ch (nreverse (trans-add trans)))
    (index-rem (change-index ch) (change-key ch) (change-rec ch)
               :trans nil))

  (dolist (ch (nreverse (trans-rem trans)))
    (index-add (change-index ch) (change-rec ch)
               :key (change-key ch)
               :trans nil))

  (trans-clear trans))

(defun index-len (self)
  ;; Returns length of SELF
  (slist-len (idx-recs self)))

(defun index-match (self other &optional prev-match)
  ;; Returns next matching records from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH.  
  (slist-match (idx-recs self) (idx-recs other) prev-match))

(defun index-merge (self other)
  ;; Adds all records from OTHER that are not found in SELF and
  ;; returns SELF.
  (slist-merge (idx-recs self) (idx-recs other)))
