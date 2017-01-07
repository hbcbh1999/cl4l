(defpackage cl4l-slist
  (:export make-slist
           slist slist-add slist-clone slist-del
           slist-diff slist-find slist-first slist-join slist-key
           slist-last slist-len slist-prev slist-rem
           slist-tests)
  (:import-from cl4l-utils compare do-bench)
  (:use cl))

(in-package cl4l-slist)

(defstruct (slist (:conc-name sl-) (:constructor make-sl)) 
  (head nil) 
  (key nil) 
  (len 0) 
  (tail nil))

(defun make-slist (&rest args)
  ;; Returns a new slist from ARGS
  (let ((lst (apply #'make-sl args)))
    (unless (sl-tail lst)
      (setf (sl-tail lst) (last (sl-head lst))))
    lst))

(defun slist (key &rest its)
  ;; Returns a new slist with KEY, initialized from ITS
  (let ((sits (and its (stable-sort its 
                                    (lambda (x y) 
                                      (= (compare x y) -1)) 
                                    :key key))))
    (make-slist :head (cons nil sits) 
		:len (length sits)
		:key key)))

(defun slist-clone (self)
  ;; Returns a shallow clone of SELF
  (let ((its (copy-list (sl-head self))))
    (make-slist :key (sl-key self) 
		:head its 
		:len (sl-len self))))

(defun slist-key (self it)
  ;; Returns the key for IT in SELF
  (let ((key (sl-key self)))
    (if key (funcall key it) it)))

(defun (setf slist-key) (key self)
  ;; Sets KEY in SELF
  (setf (sl-key self) key))

(defun slist-first (self &key key)
  ;; Returns all items in SELF, optionally from KEY incl.
  (rest (if key (slist-prev self key) (sl-head self))))

(defun slist-last (self)
  ;; Returns the last item from SELF
  (sl-tail self))

(defun slist-len (self)
  ;; Returns the length of SELF
  (sl-len self))

(defun slist-prev (self key &key (start (sl-head self)))
  ;; Returns the previous item in SELF matching KEY,
  ;; from START excl.
  (if (null (rest start))
      (values start nil 0)
      (let* ((lit (sl-tail self))
	     (lit-cmp (compare key 
                               (slist-key self (first lit)))))
	(if  (> lit-cmp 0)
	     (values lit (zerop lit-cmp) (sl-len self))
	     (do ((its start (rest its))
		  (pos 0 (1+ pos)))
		 ((null (rest its)) 
		  (values (sl-tail self) nil pos))
	       (let ((cmp (compare key 
                                   (slist-key 
                                    self (second its)))))
		 (when (< cmp 1)
		   (return 
		     (values its (zerop cmp) pos)))))))))

(defun slist-find (self key &key (start (sl-head self)))
  ;; Returns item with KEY in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found?) 
      (slist-prev self key :start start)
    (when found? (first (rest prev)))))

(defun slist-pos (self key &key (start (sl-head self)))
  ;; Returns the position of KEY in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found? pos) 
      (slist-prev self key :start start)
    (declare (ignore prev))
    (when found? pos)))

(defun slist-add (self it &key (start (sl-head self)))
  ;; Adds IT to SELF after START and returns IT
  (let* ((prev (slist-prev self (slist-key self it) :start start))
	 (its (push it (rest prev))))
    (when (eq prev (sl-tail self))
      (setf (sl-tail self) its))
    (incf (sl-len self))
    it))

(defun slist-del (self prev)
  ;; Deletes item after PREV from SELF returns it
  (when (eq (rest prev) (sl-tail self))
    (setf (sl-tail self) prev))
  (let ((it (second prev)))
    (pop (rest prev))
    (decf (sl-len self))
    it))

(defun slist-rem (self key &key (start (sl-head self)))
  ;; Removes KEY from SELF after START and returns item
  (multiple-value-bind (prev found?) 
      (slist-prev self key :start start)
    (when found? (slist-del self prev))))

(defun slist-match (self other
                    &optional prev-match)
  ;; Returns next matching items from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH
  (unless prev-match
    (setf prev-match (cons (sl-head self) (sl-head other))))
  (let* ((start (first prev-match)) (prev-iit start))
    (do ((iit (rest start)) 
         (jit (rest (rest prev-match))))
        ((or (null iit) (null jit)) nil)
      (let* ((ikey (slist-key self (first iit)))
             (jkey (slist-key self (first jit)))
             (cmp (compare ikey jkey)))
        (case cmp
          (-1 
           (setf prev-iit 
                 (slist-prev self jkey :start prev-iit))
           (setf iit (rest prev-iit)))
          (1 (setf jit 
                   (rest (slist-prev other 
                                     ikey 
                                     :start jit))))
          (t
           (return (cons prev-iit jit))))))))

(defun slist-join (self other)
  ;; Removes all items from SELF that are not found in OTHER and
  ;; returns SELF.
  (do ((m nil) (pm nil m)) (nil)
    (setf m (slist-match self other m))
    (when pm
      (do ((it (second (first m)))
           (pits (rest (first pm))))
          ((eq it (second pits)) nil)
        (slist-del self pits)))
    (unless m (return self))))

(defun slist-diff (self other)
  ;; Removes all items from SELF that are found in OTHER and
  ;; returns SELF.
  (do ((m nil) (pm nil m)) (nil)
    (setf m (slist-match self other m))
    (when m (slist-del self (first m)))
    (unless m (return self))))

(defmethod compare ((x slist) y)
  (compare (slist-first x) (slist-first y)))

;;; Tests

(defun match-tests ()
  (let* ((x (slist nil 1 2 3 4 5))
         (y (slist nil 3 5 6))
         (m1 (slist-match x y))
         (m2 (slist-match x y m1))
         (m3 (slist-match x y m2)))
    (assert (= 3 (second (first m1))))
    (assert (= 5 (second (first m2))))
    (assert (null m3))))

(defun diff-tests ()
  (let* ((x (slist nil 1 2 3 4 5))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-diff xy y)
    (assert (= 2 (slist-len xy)))))

(defun join-tests ()
  (let* ((x (slist nil 1 2 3 4 5 8))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-join xy y)
    (assert (= 3 (slist-len xy)))))

(defparameter max-len 10000)
(defparameter num-warmups 10)
(defparameter num-reps 5)
(defparameter push-prob 10)
(defparameter swap-prob 3)

(defun rnd-list ()
  (let ((lst) (len 0))
    (dotimes (i max-len) 
      (unless (zerop (random push-prob)) 
	(push i lst)
	(incf len)))
    (dotimes (_ len lst)
      (do ((i lst (rest i)))
	  ((null (nthcdr 2 i)) lst)
	(if (zerop (random swap-prob))
	    (let ((tmp (second i)))
	      (setf (second i) (third i)
		    (third i) tmp))
	    (push (pop (rest i)) lst))))))

(defun rnd-slist (lst)
  (apply #'slist nil lst))

(defun perf-tests ()
  (let ((x (rnd-list)) (y (rnd-list)))
    (do-bench (num-warmups num-reps)
      (let ((join (copy-list x))
	    (diffxy (copy-list x))
	    (diffyx (copy-list y)))
	(setf join (nintersection join y :test #'=))
	(setf diffxy (nset-difference diffxy y :test #'=))
	(setf diffyx (nset-difference diffyx x :test #'=))))
    (let ((xs (rnd-slist x)) (ys (rnd-slist y)))
      (do-bench (num-warmups num-reps)
	(let ((join (slist-clone xs)) 
	      (diffxy (slist-clone xs))
	      (diffyx (slist-clone ys)))
	  (slist-join join ys)
	  (slist-diff diffxy ys)
	  (slist-diff diffyx xs)
	  (assert (= (+ (slist-len xs) (slist-len ys))
		     (+ (* 2 (slist-len join))
			(slist-len diffxy)
			(slist-len diffyx)))))))))

(defun slist-tests ()
  (match-tests)
  (diff-tests)
  (join-tests)
  ;(perf-tests)
)
