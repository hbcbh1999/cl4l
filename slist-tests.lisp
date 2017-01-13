(defpackage cl4l-slist-tests
  (:use cl cl4l-slist cl4l-test))

(in-package cl4l-slist-tests)

(defstruct (rec)
  foo bar baz)

(define-test (:slist :rec)
  (let* ((lst (slist (list #'rec-foo #'rec-bar)))
         (rec1 (slist-add lst (make-rec :foo 1 :bar 2 :baz "ab")))
         (rec2 (slist-add lst (make-rec :foo 2 :bar 3 :baz "bc")))
         (rec3 (slist-add lst (make-rec :foo 3 :bar 4 :baz "cd"))))

    ;; Make sure all records were added
    (assert (and rec1 rec2 rec3))
    (assert (= 3 (slist-len lst)))
    
    ;; Find by key
    (assert (eq rec2 (slist-find lst (slist-key lst rec2))))))

(define-test (:slist :clone)
  (let ((lst (slist nil)))
    (dotimes (i 10) (slist-add lst i))
    (let ((clone (slist-clone lst)))
      (dotimes (i 10) (assert (slist-rem clone
                                         (slist-key clone i))))
      (assert (zerop (slist-len clone))))
    (assert (= 10 (slist-len lst)))))

(define-test (:slist :multi)
  (let ((lst (make-slist :key (list #'first #'second)
                         :uniq? nil)))
    (slist-add lst '(1 2 3))
    (slist-add lst '(4 5 6))
    
    ;; Duplicate records are not allowed
    (assert (null (slist-add lst '(1 2 3))))
    
    (let* ((rec '(1 2 4))
           (key (slist-key lst rec)))
      ;; But same key in different record is fine
      (assert (slist-add lst rec))

      ;; Records are sorted within the same key using
      ;; the same generic #'COMPARE as keys
      (let ((found (slist-first lst :key key)))
        (assert (eq '(1 2 3) (first found)))
        (assert (eq rec (second found)))
        (assert (equal '(4 5 6) (third found))))

      ;; The API supports optionally specifying record as well
      ;; as key, default is first record matching key
      (assert (eq rec (slist-find lst key :it rec)))
      (slist-rem lst key :it rec)
      (assert (null (slist-find lst key :it rec)))
      (assert (equal '(1 2 3) (slist-find lst key))))))

(define-test (:slist :match)
  (let* ((x (slist nil 1 2 3 4 5))
         (y (slist nil 3 5 6))
         (m1 (slist-match x y))
         (m2 (slist-match x y :prev-match m1))
         (m3 (slist-match x y :prev-match m2)))
    (assert (= 3 (second (first m1))))
    (assert (= 5 (second (first m2))))
    (assert (null m3))))

(define-test (:slist :diff)
  (let* ((x (slist nil 1 2 3 4 5))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-diff xy y)
    (assert (= 2 (slist-len xy)))))

(define-test (:slist :join)
  (let* ((x (slist nil 1 2 3 4 5 8))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-join xy y)
    (assert (= 3 (slist-len xy)))))

(define-test (:slist :merge)
  (let* ((x (slist nil 1 6 9))
	 (y (slist nil 1 2 3 4 5 7 8 9)))
    (slist-merge x y)
    (assert (= 9 (slist-len x)))))

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
         (xs (apply #'slist nil x))
         (ys (apply #'slist nil y)))
    (call-next-method)))

(define-test (:slist :perf)
  (let ((join (slist-clone xs)) 
        (diffxy (slist-clone xs))
        (diffyx (slist-clone ys))
        (merge (slist-clone xs)))
    (slist-join join ys)
    (slist-diff diffxy ys)
    (slist-diff diffyx xs)
    (slist-merge merge ys)))

(define-test (:slist :perf :built-in)
  (let ((join (copy-list x))
        (diffxy (copy-list x))
        (diffyx (copy-list y))
        (merge (copy-list x)))
    (setf join (nintersection join y))
    (setf diffxy (nset-difference diffxy y))
    (setf diffyx (nset-difference diffyx x))
    (setf merge (nunion merge y))))
