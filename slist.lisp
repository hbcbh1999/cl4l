(defpackage cl4l-slist
  (:export make-slist
           slist slist-add slist-clone slist-del
           slist-diff slist-find slist-first slist-ins slist-join
           slist-key
           slist-last slist-len slist-match slist-merge
           slist-prev slist-rem)
  (:import-from cl4l-utils compare)
  (:use cl))

(in-package cl4l-slist)

(defstruct (lst) 
  head key (len 0) tail (uniq? t))

(defun make-slist (&rest args)
  ;; Returns a new slist from ARGS
  (let ((lst (apply #'make-lst args)))
    (unless (lst-head lst)
      (setf (lst-head lst) (list nil)))
    (unless (lst-tail lst)
      (setf (lst-tail lst) (last (lst-head lst))))
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
  ;; Returns a clone of SELF
  (let ((its (copy-list (lst-head self))))
    (make-slist :key (lst-key self) 
		:head its 
		:len (lst-len self)
                :uniq? (lst-uniq? self))))

(defun slist-key (self it)
  ;; Returns the key for IT in SELF
  (let ((key (lst-key self)))
    (if key (funcall key it) it)))

(defun (setf slist-key) (key self)
  ;; Sets KEY in SELF
  (setf (lst-key self) key))

(defun slist-last (self)
  ;; Returns the last item from SELF
  (lst-tail self))

(defun slist-len (self)
  ;; Returns the length of SELF
  (lst-len self))

(defun slist-prev (self key it &key start)
  ;; Returns the previous item in SELF matching KEY/IT,
  ;; from START excl.
  (unless start (setf start (lst-head self)))
  (if (null (rest start))
      (values start nil 0)
      (let* ((lit (lst-tail self))
	     (lit-cmp (compare key 
                               (slist-key self (first lit)))))
	(if  (> lit-cmp 0)
	     (values lit (zerop lit-cmp) (lst-len self))
	     (do ((its start (rest its))
		  (pos 0 (1+ pos)))
		 ((null (rest its)) 
		  (values (lst-tail self) nil pos))
	       (let ((cmp (compare key 
                                   (slist-key 
                                    self (second its)))))
                 (when (and (zerop cmp) (not (lst-uniq? self)) it)
                   (setf cmp (compare it (second its))))
		 (when (< cmp 1)
		   (return (values its (zerop cmp) pos)))))))))

(defun slist-first (self &optional key it)
  ;; Returns all items in SELF, optionally from KEY/IT incl.
  (rest (if key (slist-prev self key it) (lst-head self))))

(defun slist-find (self key it &key start)
  ;; Returns item with KEY/IT in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found?) 
      (slist-prev self key it :start start)
    (when found? (first (rest prev)))))

(defun slist-pos (self key it &key start)
  ;; Returns the position of KEY/IT in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found? pos) 
      (slist-prev self key it :start start)
    (declare (ignore prev))
    (when found? pos)))

(defun slist-ins (self prev it)
  ;; Inserts IT after PREV in SELF and returns IT
  (let* ((its (push it (rest prev))))
    (when (eq prev (lst-tail self))
      (setf (lst-tail self) its))
    (incf (lst-len self))
    it))

(defun slist-add (self it &key (key (slist-key self it))
                               start)
  ;; Adds IT to SELF after START and returns IT
  (multiple-value-bind (prev found?)
      (slist-prev self key it :start start)
    (unless (and found?
                 (or (lst-uniq? self)
                     (eq it (second prev))))
      (slist-ins self prev it))))

(defun slist-del (self prev)
  ;; Deletes item after PREV from SELF returns it
  (when (eq (rest prev) (lst-tail self))
    (setf (lst-tail self) prev))
  (let ((it (second prev)))
    (pop (rest prev))
    (decf (lst-len self))
    it))

(defun slist-rem (self key it &key start)
  ;; Removes KEY/IT from SELF after START and returns item
  (multiple-value-bind (prev found?) 
      (slist-prev self key it :start start)
    (when found? (slist-del self prev))))

(defun slist-match (self other &optional prev-match)
  ;; Returns next matching items from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH.
  (unless prev-match
    (setf prev-match (cons (lst-head self) (lst-head other))))
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
                 (slist-prev self jkey (first jit)
                             :start prev-iit))
           (setf iit (rest prev-iit)))
          (1 (setf jit 
                   (rest (slist-prev other ikey (first iit) 
                                     :start jit))))
          (t
           (return (cons prev-iit jit))))))))

(defun slist-join (self other)
  ;; Removes all items from SELF that are not found in OTHER and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (prev (lst-head self) (rest (first m)))) (nil)
    (setf m (slist-match self other m))
    
    (do ((it (second (first m))))
        ((eq it (second prev)) nil)
      (slist-del self prev))
    
    (unless m (return self))))

(defun slist-diff (self other)
  ;; Removes all items from SELF that are found in OTHER and
  ;; returns SELF.
  (do ((m nil)) (nil)
    (setf m (slist-match self other m))
    (when m (slist-del self (first m)))
    (unless m (return self))))

(defun slist-merge (self other)
  ;; Adds all items from OTHER that are not found in SELF and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (start (lst-head self) (rest (first m)))
       (prev (slist-first other) (rest (rest m)))) (nil)
    (setf m (slist-match self other m))

    (do ((it (first (rest m)))
         (pits prev (rest pits)))
        ((or (null pits)
             (eq it (first pits))))
      (slist-ins self start (first pits))
      (setf start (rest start)))

    (unless m (return self))))

(defmethod compare ((x lst) y)
  (compare (slist-first x) (slist-first y)))
