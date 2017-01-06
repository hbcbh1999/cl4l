(defpackage cl4l-index
  (:export make-index
           index-add index-commit index-find index-key index-len
           index-rollback
           index-tests)
  (:import-from cl4l-macro-utils with-gsyms)
  (:import-from cl4l-utils do-hash-table)
  (:use cl4l-slist common-lisp))

(in-package cl4l-index)

(defvar *trans* nil)

(defmacro do-index (&body body)
  `(let ((*trans* (make-tr)))
     (unwind-protect
          (progn
            ,@body
            (index-commit))
       (index-rollback))))

(defstruct (index (:conc-name idx-) (:constructor make-idx)) 
  (keys nil)
  (name (gensym))
  (recs nil)
  (uniq? nil))

(defstruct (trans (:conc-name tr-) (:constructor make-tr))
  (add (make-hash-table :test #'eq))
  (rem (make-hash-table :test #'eq)))

(defun index-key (self rec)
  (mapcar (lambda (fn) (funcall fn rec)) (idx-keys self)))

(defun make-index (&rest args)
  (let ((idx (apply #'make-idx args)))
    (setf (idx-recs idx)
          (slist (lambda (rec)
                   (index-key idx rec))))
    idx))

(defun index-find (self key)
  (slist-find (idx-recs self) key))

(defun index-add (self rec &key (trans *trans*))
  (let* ((key (index-key self rec))
         (found (index-find self key)))
    (when (and rec trans)
      (setf (gethash (cons self rec) (tr-add trans)) key)
      (let ((found (gethash (cons self rec) (tr-add trans))))
        (when (equal found key)
          (remhash (cons self rec) (tr-rem trans)))))
    (unless (and found (idx-uniq? self))
      (slist-add (idx-recs self) rec))))

(defun index-commit (&key (trans *trans*))
  (clrhash (tr-add trans)))

(defun index-rem (self key &key (trans *trans*))
  (let ((rec (slist-rem (idx-recs self) key)))
    (when (and rec trans)
      (setf (gethash (cons self rec) (tr-rem trans)) key)
      (let ((found (gethash (cons self rec) (tr-add trans))))
        (when (equal found key)
          (remhash (cons self rec) (tr-add trans)))))))

(defun index-rollback (&key (trans *trans*))
  (do-hash-table ((tr-add trans) k v)
    (index-rem (first k) v :trans nil)))

(defun index-len (self)
  (slist-len (idx-recs self)))

(defstruct (rec)
  foo bar baz)

(defun basic-tests ()
  (let* ((idx (make-index :keys (list #'rec-foo #'rec-bar)
                          :uniq? t))
         (rec1 (index-add idx (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (index-add idx (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (index-add idx (make-rec :foo 3 :bar 4 :baz "cd"))))
    (assert (and rec1 rec2 rec3))
    (assert (null (index-add idx rec1)))
    (assert (= 3 (index-len idx)))
    (assert (eq rec1 (index-find idx (index-key idx rec1))))
    (assert (eq rec2 (index-find idx (index-key idx rec2))))
    (assert (eq rec3 (index-find idx (index-key idx rec3))))))

(defun rollback-tests ()
  (let ((idx (make-index :keys (list #'rec-foo) :uniq? t)))
    (do-index
      (let ((rec (index-add idx (make-rec :foo 1 :baz "ab"))))
        (index-rollback)
        (assert (= 0 (index-len idx)))
        (assert (null (index-find idx (index-key idx rec))))))))

(defun index-tests ()
  (basic-tests)
  (rollback-tests))

