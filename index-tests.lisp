(defpackage cl4l-index-tests
  (:import-from cl4l-test define-test)
  (:use cl cl4l-index))

(in-package cl4l-index-tests)

(defstruct (rec)
  foo bar baz)

(define-test (:index :basic)
  (let* ((idx (make-index (list #'rec-foo #'rec-bar)))
         (rec1 (index-add idx (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (index-add idx (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (index-add idx (make-rec :foo 3 :bar 4 :baz "cd"))))
    (assert (and rec1 rec2 rec3))
    (assert (= 3 (index-len idx)))
    (assert (eq rec1 (index-find idx (index-key idx rec1))))
    (assert (eq rec2 (index-find idx (index-key idx rec2))))
    (assert (eq rec3 (index-find idx (index-key idx rec3))))))

(define-test (:index :clone)
  (let ((idx (make-index (list #'identity))))
    (dotimes (i 10) (index-add idx i))
    (let ((clone (index-clone idx)))
      (dotimes (i 10) (assert (index-rem clone
                                         (index-key clone i))))
      (assert (zerop (index-len clone))))
    (assert (= 10 (index-len idx)))))

(define-test (:index :str)
  (let* ((idx (make-index (list #'length #'identity)))
         (rec1 (index-add idx "ab"))
         (rec2 (index-add idx "cd"))
         (rec3 (index-add idx "z"))
         (recs (index-first idx)))
    (assert (string= rec3 (pop recs)))
    (assert (string= rec1 (pop recs)))
    (assert (string= rec2 (pop recs)))))

(define-test (:index :trans)
  (let ((idx (make-index (list #'rec-foo))))
    (with-index
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

(define-test (:index :update)
  (let* ((idx (make-index (list #'first) :uniq? t))
         (rec (index-add idx '(41))))
    (with-index
      (index-add idx rec)
      (incf (first rec))
      (index-add idx rec))
    (assert (= 1 (index-len idx)))))
