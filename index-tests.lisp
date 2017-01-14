(defpackage cl4l-index-tests
  (:use cl cl4l-index cl4l-test))

(in-package cl4l-index-tests)

(defstruct (rec)
  foo bar baz)

(define-test (:index :rec)
  (let* ((lst (index (list #'rec-foo #'rec-bar)))
         (rec1 (index-add lst (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (index-add lst (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (index-add lst (make-rec :foo 3 :bar 4 :baz "cd"))))

    ;; Make sure all records were added
    (assert (and rec1 rec2 rec3))
    (assert (= 3 (index-length lst)))
    
    ;; Find by key
    (assert (eq rec2 (index-find lst (index-key lst rec2))))))

(define-test (:index :clone)
  (let ((lst (index nil)))
    (dotimes (i 10) (index-add lst i))
    (let ((clone (index-clone lst)))
      (dotimes (i 10) (assert (index-remove clone
                                            (index-key clone i))))
      (assert (zerop (index-length clone))))
    (assert (= 10 (index-length lst)))))

(define-test (:index :multi)
  (let ((lst (make-index :key (list #'first #'second)
                         :unique? nil)))
    (index-add lst '(1 2 3))
    (index-add lst '(4 5 6))
    
    ;; Duplicate records are not allowed
    (assert (null (index-add lst '(1 2 3))))
    
    (let* ((rec '(1 2 4))
           (key (index-key lst rec)))
      ;; But same key in different record is fine
      (assert (index-add lst rec))

      ;; Records are sorted within the same key using
      ;; the same generic #'COMPARE as keys
      (let ((found (index-first lst :key key)))
        (assert (eq '(1 2 3) (first found)))
        (assert (eq rec (second found)))
        (assert (equal '(4 5 6) (third found))))

      ;; The API supports optionally specifying record as well
      ;; as key, default is first record matching key
      (assert (eq rec (index-find lst key :it rec)))
      (index-remove lst key :it rec)
      (assert (null (index-find lst key :it rec)))
      (assert (equal '(1 2 3) (index-find lst key))))))

(define-test (:index :trans)
  (let ((lst (index (list #'rec-foo #'rec-bar))))

    ;; Start new transaction that is automatically
    ;; rolled back on early and committed on
    ;; normal exit
    (with-index-trans ()
      (let* ((rec (make-rec :foo 1 :bar 2 :baz "ab"))
             (key (index-key lst rec)))
        (index-add lst rec)

        ;; Rollback and make sure record is gone
        (index-rollback)
        (assert (= 0 (index-length lst)))
        (assert (null (index-find lst key)))

        ;; Add again, commit and remove
        (index-add lst rec)
        (index-commit)
        (index-remove lst key)

        ;; Rollback and make sure record is still there
        (index-rollback)
        (assert (eq rec (index-find lst key)))))))

(define-test (:index :match)
  (let* ((x (index nil 1 2 3 4 5))
         (y (index nil 3 5 6))
         (m1 (index-match x y))
         (m2 (index-match x y :prev-match m1))
         (m3 (index-match x y :prev-match m2)))
    (assert (= 3 (second (first m1))))
    (assert (= 5 (second (first m2))))
    (assert (null m3))))

(define-test (:index :diff)
  (let* ((x (index nil 1 2 3 4 5))
	 (y (index nil 1 3 5 6 7))
	 (xy (index-clone x)))
    (index-diff xy y)
    (assert (= 2 (index-length xy)))))

(define-test (:index :join)
  (let* ((x (index nil 1 2 3 4 5 8))
	 (y (index nil 1 3 5 6 7))
	 (xy (index-clone x)))
    (index-join xy y)
    (assert (= 3 (index-length xy)))))

(define-test (:index :merge)
  (let* ((x (index nil 1 6 9))
	 (y (index nil 1 2 3 4 5 7 8 9)))
    (index-merge x y)
    (assert (= 9 (index-length x)))))

(defparameter len 1000)

(defun rnd-list ()
  (let ((lst))
    (dotimes (_ len)
      (push (random (* len len)) lst))
    lst))

(defparameter x nil)
(defparameter y nil)
(defparameter xs nil)
(defparameter ys nil)

(defmethod run-suite :around (run &key)
  (let* ((x (rnd-list))
         (y (rnd-list))
         (xs (apply #'index nil x))
         (ys (apply #'index nil y)))
    (call-next-method)))

(define-test (:index :perf)
  (let ((join (index-clone xs)) 
        (diffxy (index-clone xs))
        (diffyx (index-clone ys))
        (merge (index-clone xs)))
    (index-join join ys)
    (index-diff diffxy ys)
    (index-diff diffyx xs)
    (index-merge merge ys)))

(define-test (:index :perf :built-in)
  (let ((join (copy-list x))
        (diffxy (copy-list x))
        (diffyx (copy-list y))
        (merge (copy-list x)))
    (setf join (nintersection join y))
    (setf diffxy (nset-difference diffxy y))
    (setf diffyx (nset-difference diffyx x))
    (setf merge (nunion merge y))))
