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

(defstruct (slist (:conc-name sl-) (:constructor make-sl)) 
  head 
  key 
  (len 0) 
  tail
  uniq?)

(defun make-slist (&rest args)
  ;; Returns a new slist from ARGS
  (let ((lst (apply #'make-sl args)))
    (unless (sl-head lst)
      (setf (sl-head lst) (list nil)))
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
		:len (sl-len self)
                :uniq? (sl-uniq? self))))

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

(defun slist-ins (self prev it)
  ;; Inserts IT after PREV in SELF and returns IT
  (let* ((its (push it (rest prev))))
    (when (eq prev (sl-tail self))
      (setf (sl-tail self) its))
    (incf (sl-len self))
    it))

(defun slist-add (self it &key (key (slist-key self it))
                               (start (sl-head self)))
  ;; Adds IT to SELF after START and returns IT
  (multiple-value-bind (prev found?)
      (slist-prev self key :start start)
    (unless (and (sl-uniq? self)
                 found?
                 (zerop (compare it (second prev))))
      (slist-ins self prev it))))

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

(defun slist-match (self other &optional prev-match)
  ;; Returns next matching items from (SELF . OTHER)
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
  ;; returns self.
  (do ((m nil)
       (first? t nil)
       (prev (sl-head self) (rest (first m)))) (nil)
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
       (start (sl-head self) (rest (first m)))
       (prev (slist-first other) (rest (rest m)))) (nil)
    (setf m (slist-match self other m))

    (do ((it (first (rest m)))
         (pits prev (rest pits)))
        ((or (null pits)
             (eq it (first pits))))
      (slist-ins self start (first pits))
      (setf start (rest start)))

    (unless m (return self))))

(defmethod compare ((x slist) y)
  (compare (slist-first x) (slist-first y)))
