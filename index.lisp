(defpackage cl4l-index
  (:export index-add index-clone index-commit index-del index-diff
           index-find index-first
           index-join index-key index-last index-len
           index-rem index-rollback
           make-index make-trans
           with-index)
  (:import-from cl4l-macro-utils with-gsyms)
  (:import-from cl4l-utils do-hash-table)
  (:use cl cl4l-slist))

(in-package cl4l-index)

;; Default trans
(defvar *trans* nil)

(defmacro with-index (&body body)
  (with-gsyms (_res)
    `(let ((*trans* (make-trans)))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (index-commit)
                ,_res))
         (index-rollback)))))

(defstruct (index (:conc-name idx-) (:constructor make-idx)) 
  (keys nil)
  (name (gensym))
  (recs nil)
  (uniq? nil))

(defun make-trans ()
  (make-hash-table :test #'eq))

(defun index-key (self rec)
  (mapcar (lambda (fn) (funcall fn rec)) (idx-keys self)))

(defun make-index (keys &rest args)
  (let ((idx (apply #'make-idx :keys keys args)))
    (unless (idx-recs idx)
            (setf (idx-recs idx)
                  (slist (lambda (rec)
                           (index-key idx rec)))))
    idx))

(defun index-find (self key)
  (slist-find (idx-recs self) key))

(defun index-add (self rec &key (key (index-key self rec))
                                (trans *trans*))
  (let ((found (index-find self key)))
    (when trans
      (setf (gethash (cons self rec) trans) key))
    (unless (and found (idx-uniq? self))
      (slist-add (idx-recs self) rec))))

(defun index-clone (self &rest args)
  (apply #'make-index (idx-keys self)
         :uniq? (idx-uniq? self)
         :recs (slist-clone (idx-recs self))
         args))

(defun index-commit (&key (trans *trans*))
  (clrhash trans))

(defun index-del (self prev)
  (slist-del (idx-recs self) prev))

(defun index-diff (self other)
  (slist-diff (idx-recs self) (idx-recs other)))

(defun index-first (self &key key)
  (slist-first (idx-recs self) :key key))

(defun index-join (self other)
  (slist-join (idx-recs self) (idx-recs other)))

(defun index-last (self)
  (slist-last (idx-recs self)))

(defun index-rem (self key &key (trans *trans*))
  (let ((rec (slist-rem (idx-recs self) key)))
    (when (and rec trans)
      (setf (gethash (cons self rec) trans) t))
    rec))

(defun index-rollback (&key (trans *trans*))
  (do-hash-table (trans k v)
    (if (eq v t)
        (index-add (first k) (rest k) :trans nil)
        (index-rem (first k) v :trans nil)))
  (clrhash trans))

(defun index-len (self)
  (slist-len (idx-recs self)))
