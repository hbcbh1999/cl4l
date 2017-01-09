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
                                         (index-key clone i))))
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

(define-test (:index :update)
  (let* ((idx (make-index (list #'first) :uniq? t))
         (rec (index-add idx '(41))))
    (with-index-trans ()
      (index-add idx rec)
      (incf (first rec))
      (index-add idx rec))
    (assert (= 1 (index-len idx)))))

(define-test (:index :multi)
  ;; Indexes are non unique by default
  (let ((idx (make-index (list #'first #'second))))
    (index-add idx '(1 2 3))
    (index-add idx '(4 5 6))
    
    ;; Duplicate records are not allowed
    (assert (null (index-add idx '(1 2 3))))
    
    (let* ((rec '(1 2 4))
           (key (index-key idx rec)))
      ;; But same key in different record is fine
      (assert (index-add idx rec))

      ;; Records are sorted within the same key using
      ;; the same generic #'COMPARE as keys
      (let ((found (index-first idx key)))
        (assert (eq '(1 2 3) (first found)))
        (assert (eq rec (second found)))
        (assert (equal '(4 5 6) (third found))))

      ;; The api supports optionally specifying record as well
      ;; as key, default is first record matching key
      (assert (eq rec (index-find idx key :rec rec)))
      (index-rem idx key :rec rec)
      (assert (null (index-find idx key :rec rec)))
      (assert (equal '(1 2 3) (index-find idx key))))))
