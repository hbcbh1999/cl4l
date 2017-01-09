(defpackage cl4l-index-tests
  (:import-from cl4l-test define-test)
  (:use cl cl4l-index))

(in-package cl4l-index-tests)

(defstruct (rec)
  foo bar baz)

(define-test (:index :basic)
  (let* ((idx (make-index (list #'rec-foo #'rec-bar) :uniq? t))
         (rec1 (index-add idx (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (index-add idx (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (index-add idx (make-rec :foo 3 :bar 4 :baz "cd"))))

    ;; Make sure all records were added
    (assert (and rec1 rec2 rec3))
    (assert (= 3 (index-len idx)))
    
    ;; Find by key
    (assert (eq rec2 (index-find idx (index-key idx rec2))))))

(define-test (:index :clone)
  (let ((idx (make-index (list t))))
    (dotimes (i 10) (index-add idx i))
    (let ((clone (index-clone idx)))
      (dotimes (i 10) (assert (index-rem clone
                                         (index-key clone i)
                                         nil)))
      (assert (zerop (index-len clone))))
    (assert (= 10 (index-len idx)))))

(define-test (:index :str)
  (let* ((idx (make-index (list #'length t)))
         (rec1 (index-add idx "ab"))
         (rec2 (index-add idx "cd"))
         (rec3 (index-add idx "z"))
         (recs (index-first idx)))
    (assert (string= rec3 (pop recs)))
    (assert (string= rec1 (pop recs)))
    (assert (string= rec2 (pop recs)))))

(define-test (:index :trans)
  (let ((idx (make-index (list #'rec-foo #'rec-bar) :uniq? t)))

    ;; Start new transaction that is automatically
    ;; rolled back on early and committed on
    ;; normal exit
    (with-index
      (let* ((rec (make-rec :foo 1 :bar 2 :baz "ab"))
             (key (index-key idx rec)))
        ;; Add record
        (index-add idx rec)

        ;; Rollback and make sure record is gone
        (index-rollback)
        (assert (= 0 (index-len idx)))
        (assert (null (index-find idx key)))

        ;; Add again, commit and remove
        (index-add idx rec)
        (index-commit)
        (index-rem idx key nil)

        ;; Rollback and make sure record is still there
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
