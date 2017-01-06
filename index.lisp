(defpackage cl4l-index
  (:export make-index
           index-add index-find index-key index-len
           index-tests)
  (:use cl4l-field cl4l-slist common-lisp))

(in-package cl4l-index)

(defstruct (index (:conc-name idx-) (:constructor make-idx)) 
  (flds nil)
  (name (gensym))
  (recs nil)
  (uniq? nil))

(defun make-index (&rest args)
  (let ((idx (apply #'make-idx args)))
    (setf (idx-recs idx)
          (slist (lambda (rec)
                   (index-key idx rec))))
    idx))

(defun index-find (self key)
  (slist-find (idx-recs self) key))

(defun index-add (self rec)
  (let ((found (index-find self (index-key self rec))))
    (unless (and found (idx-uniq? self))
      (slist-add (idx-recs self) rec))))

(defun index-key (self rec)
  (mapcar (lambda (f) (field-val f rec)) (idx-flds self)))

(defun index-len (self)
  (slist-len (idx-recs self)))

(defstruct (rec)
  foo bar baz)

(defun index-tests ()
  (let* ((foo (make-field :get #'rec-foo))
         (bar (make-field :get #'rec-bar))
         (idx (make-index :flds (list foo bar) :uniq? t))
         (rec1 (index-add idx (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (index-add idx (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (index-add idx (make-rec :foo 3 :bar 4 :baz "cd"))))
    (assert (and rec1 rec2 rec3))
    (assert (null (index-add idx rec1)))
    (assert (= 3 (index-len idx)))
    (assert (eq rec1 (index-find idx (index-key idx rec1))))
    (assert (eq rec2 (index-find idx (index-key idx rec2))))
    (assert (eq rec3 (index-find idx (index-key idx rec3))))))
