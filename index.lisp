(defpackage cl4l-index
  (:export make-index
           index-add index-commit index-find index-first
           index-key index-last index-len
           index-rollback
           index-tests)
  (:import-from cl4l-macro-utils with-gsyms)
  (:import-from cl4l-utils do-hash-table)
  (:use cl4l-slist common-lisp))

(in-package cl4l-index)

;; Default trans
(defvar *trans* nil)

(defmacro do-index (&body body)
  (with-gsyms (_res)
    `(let ((*trans* (make-tr)))
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

(defstruct (tr)
  (add (make-hash-table :test #'eq))
  (rem (make-hash-table :test #'eq)))

(defun index-key (self rec)
  (mapcar (lambda (fn) (funcall fn rec)) (idx-keys self)))

(defun make-index (keys &rest args)
  (let ((idx (apply #'make-idx :keys keys args)))
    (setf (idx-recs idx)
          (slist (lambda (rec)
                   (index-key idx rec))))
    idx))

(defun index-find (self key)
  (slist-find (idx-recs self) key))

(defun index-add (self rec &key (key (index-key self rec))
                                (trans *trans*))
  (let ((found (index-find self key)))
    (when trans
      (setf (gethash (cons self rec) (tr-add trans)) key)
      (let ((rem (gethash (cons self rec) (tr-rem trans))))
        (when (equal rem key)
          (remhash (cons self rec) (tr-rem trans)))))
    (unless (and found (idx-uniq? self))
      (slist-add (idx-recs self) rec))))

(defun index-commit (&key (trans *trans*))
  (clrhash (tr-add trans))
  (clrhash (tr-rem trans)))

(defun index-first (self &key key)
  (slist-first (idx-recs self) :key key))

(defun index-last (self)
  (slist-last (idx-recs self)))

(defun index-rem (self key &key (trans *trans*))
  (let ((rec (slist-rem (idx-recs self) key)))
    (when (and rec trans)
      (setf (gethash (cons self rec) (tr-rem trans)) key)
      (let ((found (gethash (cons self rec) (tr-add trans))))
        (when (equal found key)
          (remhash (cons self rec) (tr-add trans)))))
    rec))

(defun index-rollback (&key (trans *trans*))
  (do-hash-table ((tr-add trans) k v)
    (index-rem (first k) v :trans nil))
  (clrhash (tr-add trans))

  (do-hash-table ((tr-rem trans) k v)
    (index-add (first k) (rest k) :key v :trans nil))
  (clrhash (tr-rem trans)))

(defun index-len (self)
  (slist-len (idx-recs self)))

(defstruct (rec)
  foo bar baz)

(defun basic-tests ()
  (let* ((idx (make-index (list #'rec-foo #'rec-bar)
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

(defun str-tests ()
  (let* ((idx (make-index (list #'length #'identity)
                          :uniq? t))
         (rec1 (index-add idx "ab"))
         (rec2 (index-add idx "cd"))
         (rec3 (index-add idx "z"))
         (recs (index-first idx)))
    (assert (string= rec3 (pop recs)))
    (assert (string= rec1 (pop recs)))
    (assert (string= rec2 (pop recs)))))

(defun trans-tests ()
  (let ((idx (make-index (list #'rec-foo) :uniq? t)))
    (do-index
      (let* ((rec (index-add idx (make-rec :foo 1 :baz "ab")))
             (key (index-key idx rec)))
        (index-rollback)
        (assert (= 0 (index-len idx)))
        (assert (null (index-find idx key)))
        (assert (index-add idx rec))
        (index-commit)
        (assert (eq rec (index-find idx key)))
        (assert (eq rec (index-rem idx key)))
        (index-rollback)
        (assert (eq rec (index-find idx key)))))))

(defun update-tests ()
  (let* ((idx (make-index (list #'first) :uniq? t))
         (rec (index-add idx '(41))))
    (do-index
      (index-add idx rec)
      (incf (first rec))
      (index-add idx rec))
    (assert (= 1 (index-len idx)))))

(defun index-tests ()
  (basic-tests)
  (str-tests)
  (trans-tests))

