(defpackage cl4l-slist
  (:export make-slist
           slist slist-add slist-cmp slist-clone
           slist-diff slist-find slist-first slist-join slist-key
           slist-last slist-len slist-prev slist-rem
           slist-tests)
  (:import-from cl4l-utils do-bench)
  (:use common-lisp))

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

(defgeneric slist-cmp (x y)
  ;; Specifies how to compare items,
  ;; returns result of comparison (-1, 0 or 1).
  (:method ((x character) y)
    (cond
      ((char< x y) -1)
      ((char> x y) 1)
      (t 0)))

  (:method ((x list) y)
    (do ((xi x (rest xi)) (yi y (rest yi)))
	((and (null xi) (null yi)) 0)
      (let ((cmp (slist-cmp (first xi) (first yi))))
	(unless (zerop cmp)
	  (return cmp)))))

  (:method ((x number) y)
    (cond
      ((< x y) -1)
      ((> x y) 1)
      (t 0)))

  (:method ((x slist) y)
    (slist-cmp (rest (sl-head x)) (rest (sl-head y))))

  (:method ((x string) y)
    (do ((i 0 (1+ i)))
	((= i (min (length x) (length y)))
	 (slist-cmp (length x) (length y)))
      (let* ((xc (aref x i))
	     (yc (aref y i))
	     (cmp (slist-cmp xc yc)))
	(unless (zerop cmp) (return cmp))))))

(defun slist (key &rest its)
  ;; Returns a new slist with KEY, initialized from ITS
  (let ((sits (and its (stable-sort its 
                                    (lambda (x y) 
                                      (= (slist-cmp x y) -1)) 
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

(defun slist-first (self &key key)
  ;; Returns all items in self, optionally from KEY
  (rest (if key (slist-prev self key) (sl-head self))))

(defun slist-last (self)
  ;; Returns the last item from SELF
  (sl-tail self))

(defun slist-len (self)
  ;; Returns the length of SELF
  (sl-len self))

(defun slist-prev (self key &key (start (sl-head self)))
  ;; Returns the previous item in SELF matching KEY,
  ;; from START (exclusive).
  (if (null (rest start))
      (values start nil 0)
      (let* ((lit (sl-tail self))
	     (lit-cmp (slist-cmp key 
				 (slist-key self 
					    (first lit)))))
	(if  (> lit-cmp 0)
	     (values lit (zerop lit-cmp) (sl-len self))
	     (do ((its start (rest its))
		  (pos 0 (1+ pos)))
		 ((null (rest its)) 
		  (values (sl-tail self) nil pos))
	       (let ((cmp (slist-cmp key 
				     (slist-key 
				      self (second its)))))
		 (when (< cmp 1)
		   (return 
		     (values its (zerop cmp) pos)))))))))

(defun slist-find (self key &key (start (sl-head self)))
  ;; Returns item with KEY in SELF, from START;
  ;; or NIL if not found.
  (multiple-value-bind (prev found?) 
      (slist-prev self key :start start)
    (when found? (first (rest prev)))))

(defun slist-pos (self key &key (start (sl-head self)))
  ;; Returns the position of KEY in SELF, from START;
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

(defun slist-rem (self key &key (start (sl-head self)))
  ;; Removes KEY from SELF after START and returns item
  (multiple-value-bind (prev found?) 
      (slist-prev self key :start start)
    (when found?
      (when (eq (rest prev) (sl-tail self))
	(setf (sl-tail self) prev))
      (let ((it (second prev)))
	(pop (rest prev))
	(decf (sl-len self))
	it))))

(defun slist-match (self other
                    &optional prev-match)
  ;; Returns next matching items from (SELF . OTHER)
  (unless prev-match (setf prev-match (cons (sl-head self)
                                            (sl-head other))))
  (let* ((start (first prev-match)) (prev-iit start))
    (do ((iit (rest start)) 
         (jit (rest (rest prev-match))))
        ((or (null iit) (null jit)) nil)
      (let* ((ikey (slist-key self (first iit)))
             (jkey (slist-key self (first jit)))
             (cmp (slist-cmp ikey jkey)))
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
           (return (cons iit jit))))))))

(defun slist-join (self other)
  ;; Removes all items from SELF that are found in OTHER,
  ;; from START; returns number of removed items.
  (do ((m nil) (pm nil m)) (nil)
    (setf m (slist-match self other m))
    (when pm
      (do ((it (first (first m)))
           (pits (first pm)))
          ((eq it (first (rest pits))) nil)
        (decf (sl-len self))
        (let ((pit (pop (rest pits))))
          (when (eq pit (sl-tail self))
            (setf (sl-tail self) (first (rest pits)))))))
    (unless m (return self))))

(defun slist-diff (self other &key (start (sl-head self)))
  ;; Removes all items from SELF that are found in OTHER,
  ;; from START; returns number of removed items.
  (if (or (null (rest start)) (zerop (sl-len other)))
      0
      (let ((ndel 0) (prev start))
	(do ((iit (rest start)) 
	     (jit (rest (sl-head other))))
	    ((or (null iit) (null jit)) ndel)
	  (let* ((ikey (slist-key self (first iit)))
		 (jkey (slist-key self (first jit)))
		 (cmp (slist-cmp ikey jkey)))
	    (case cmp
	      (-1 
	       (setf prev 
		     (slist-prev self jkey :start iit))
	       (setf iit (rest prev)))
	      (1 (setf jit 
		       (rest (slist-prev other 
					 ikey 
					 :start jit))))
	      (t (pop (rest prev))
               (decf (sl-len self))
               (incf ndel)
               (when (eq iit (sl-tail self))
                 (setf (sl-tail self) prev))
               (setf iit (rest iit))
               (setf jit (rest jit)))))))))

;;; Tests

(defun match-tests ()
  (let* ((x (slist nil 1 2 3 4 5))
         (y (slist nil 3 5 6))
         (m1 (slist-match x y))
         (m2 (slist-match x y m1))
         (m3 (slist-match x y m2)))
    (assert (= 3 (first (first m1))))
    (assert (= 5 (first (first m2))))
    (assert (null m3))))

(defun diff-tests ()
  (let* ((x (slist nil 1 2 3 4 5))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-diff xy y)
    (assert (= 2 (slist-len xy)))))

(defun join-tests ()
  (let* ((x (slist nil 1 2 3 4 5))
	 (y (slist nil 1 3 5 6 7))
	 (xy (slist-clone x)))
    (slist-join xy y)
    (assert (= 3 (slist-len xy)))))

(defparameter max-len 10000)
(defparameter num-warmups 3)
(defparameter num-reps 10)

(defun rnd-list ()
  (let ((lst) (len 0))
    (dotimes (i max-len) 
      (unless (zerop (random 2)) 
	(push i lst)
	(incf len)))
    (dotimes (_ len lst)
      (do ((i lst (rest i)))
	  ((null (nthcdr 2 i)) lst)
	(if (zerop (random 2))
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
