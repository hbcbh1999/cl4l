(defpackage cl4l-index-tests
  (:use cl cl4l-index cl4l-test))

(in-package cl4l-index-tests)

(defstruct (rec)
  foo bar baz)

(define-test (:index :trans)
  (let ((idx (make-index :key (list #'rec-foo #'rec-bar))))

    ;; Start new transaction that is automatically
    ;; rolled back on early and committed on
    ;; normal exit
    (with-index-trans ()
      (let* ((rec (make-rec :foo 1 :bar 2 :baz "ab"))
             (key (index-key idx rec)))
        (index-add idx rec)

        ;; Rollback and make sure record is gone
        (index-rollback)
        (assert (= 0 (index-len idx)))
        (assert (null (index-find idx key)))

        ;; Add again, commit and remove
        (index-add idx rec)
        (index-commit)
        (index-rem idx key)

        ;; Rollback and make sure record is still there
        (index-rollback)
        (assert (eq rec (index-find idx key)))))))
