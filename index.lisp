(defpackage cl4l-index
  (:export make-index make-index-trans
           index index-add index-clone index-commit index-delete
           index-diff index-find index-first index-insert
           index-join index-key
           index-last index-length index-match index-merge
           index-prev index-remove index-rollback
           with-index-trans *index-trans*)
  (:shadowing-import-from cl4l-utils compare key-gen with-symbols)
  (:use cl))

(in-package cl4l-index)

;; Default trans
(defvar *index-trans* nil)

(defmacro with-index-trans ((&key trans) &body body)
  ;; Executes BODY in transaction that is automatically
  ;; rolled back on early and committed on normal exit
  (with-symbols (_res)
    `(let ((*index-trans* (or ,trans (make-index-trans))))
       (unwind-protect
            (progn
              (let ((,_res (progn ,@body)))
                (index-commit)
                ,_res))
         (index-rollback)))))

(defstruct (idx) 
  head key-gen (length 0) tail (unique? t))

(defstruct (ch)
  op index key it)

(defun make-index (&key key
                        key-gen
                        (head (list nil))
                        tail
                        (length 0)
                        (unique? t))
  ;; Returns a new index from ARGS
  (make-idx :key-gen (or key-gen (key-gen key))
            :head head
            :tail (or tail (last head))
            :length length
            :unique? unique?))

(defun make-index-trans ()
  (list nil))

(defun index (key &rest its)
  ;; Returns a new index with KEY, initialized from ITS
  (let ((sits (and its (stable-sort its 
                                    (lambda (x y) 
                                      (= (compare x y) -1)) 
                                    :key key))))
    (make-index :key key
                :head (cons nil sits) 
                :length (length sits))))

(defun index-clone (self)
  ;; Returns a clone of SELF
  (let ((its (copy-list (idx-head self))))
    (make-index :key-gen (idx-key-gen self) 
		:head its 
		:length (idx-length self)
                :unique? (idx-unique? self))))

(defun index-key (self it)
  ;; Returns key for IT in SELF
  (funcall (idx-key-gen self) it))

(defun (setf index-key) (key self)
  ;; Sets KEY in SELF
  (setf (idx-key-gen self) (key-gen key)))

(defun index-last (self)
  ;; Returns the last item from SELF
  (idx-tail self))

(defun index-length (self)
  ;; Returns the length of SELF
  (idx-length self))

(defun index-prev (self key &key it start)
  ;; Returns the previous item in SELF matching KEY/IT,
  ;; from START excl.
  (unless start (setf start (idx-head self)))
  (if (null (rest start))
      (values start nil 0)
      (let* ((lit (idx-tail self))
	     (lit-cmp (compare key 
                               (index-key self (first lit)))))
	(if  (> lit-cmp 0)
	     (values lit (zerop lit-cmp) (idx-length self))
	     (do ((its start (rest its))
		  (pos 0 (1+ pos)))
		 ((null (rest its)) 
		  (values (idx-tail self) nil pos))
	       (let ((cmp (compare key 
                                   (index-key 
                                    self (second its)))))
                 (when (and (zerop cmp)
                            (not (idx-unique? self))
                            it)
                   (setf cmp (compare it (second its))))
		 (when (< cmp 1)
		   (return (values its (zerop cmp) pos)))))))))

(defun index-first (self &key key it)
  ;; Returns all items in SELF, optionally from KEY/IT incl.
  (rest (if key (index-prev self key :it it) (idx-head self))))

(defun index-find (self key &key it start)
  ;; Returns item with KEY/IT in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found?) 
      (index-prev self key :it it :start start)
    (when found? (first (rest prev)))))

(defun index-pos (self key it &key start)
  ;; Returns the position of KEY/IT in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found? pos) 
      (index-prev self key :it it :start start)
    (declare (ignore prev))
    (when found? pos)))

(defun index-insert (self prev it)
  ;; Inserts IT after PREV in SELF and returns IT
  (let* ((its (push it (rest prev))))
    (when (eq prev (idx-tail self))
      (setf (idx-tail self) its))
    (incf (idx-length self))
    it))

(defun index-add (self it &key (key (index-key self it))
                               start
                               (trans *index-trans*))
  ;; Adds IT to SELF after START and returns IT
  (multiple-value-bind (prev found?)
      (index-prev self key :it it :start start)
    (unless (and found?
                 (or (idx-unique? self)
                     (eq it (second prev))))
      (when trans
        (push (make-ch :op :add :index self :key key :it it)
              (rest trans)))   
      (index-insert self prev it))))

(defun index-delete (self prev)
  ;; Deletes item after PREV from SELF returns it
  (when (eq (rest prev) (idx-tail self))
    (setf (idx-tail self) prev))
  (let ((it (second prev)))
    (pop (rest prev))
    (decf (idx-length self))
    it))

(defun index-remove (self key &key it start (trans *index-trans*))
  ;; Removes KEY/IT from SELF after START and returns item
  (multiple-value-bind (prev found?) 
      (index-prev self key :it it :start start)
    (when found?
      (when trans
        (push (make-ch :op :remove
                       :index self
                       :key key :it (second prev))
              (rest trans)))
      (index-delete self prev))))

(defun index-match (self other &key prev-match)
  ;; Returns next matching items from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH.
  (unless prev-match
    (setf prev-match (cons (idx-head self) (idx-head other))))
  (let* ((start (first prev-match)) (prev-iit start))
    (do ((iit (rest start)) 
         (jit (rest (rest prev-match))))
        ((or (null iit) (null jit)) nil)
      (let* ((ikey (index-key self (first iit)))
             (jkey (index-key self (first jit)))
             (cmp (compare ikey jkey)))
        (case cmp
          (-1 
           (setf prev-iit 
                 (index-prev self jkey
                             :it (first jit)
                             :start prev-iit))
           (setf iit (rest prev-iit)))
          (1 (setf jit 
                   (rest (index-prev other ikey
                                     :it (first iit) 
                                     :start jit))))
          (t
           (return (cons prev-iit jit))))))))

(defun index-join (self other)
  ;; Removes all items from SELF that are not found in OTHER and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (prev (idx-head self) (rest (first m)))) (nil)
    (setf m (index-match self other :prev-match m))
    
    (do ((it (second (first m))))
        ((eq it (second prev)) nil)
      (index-delete self prev))
    
    (unless m (return self))))

(defun index-diff (self other)
  ;; Removes all items from SELF that are found in OTHER and
  ;; returns SELF.
  (do ((m nil)) (nil)
    (setf m (index-match self other :prev-match m))
    (when m (index-delete self (first m)))
    (unless m (return self))))

(defun index-merge (self other)
  ;; Adds all items from OTHER that are not found in SELF and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (start (idx-head self) (rest (first m)))
       (prev (index-first other) (rest (rest m)))) (nil)
    (setf m (index-match self other :prev-match m))

    (do ((it (first (rest m)))
         (pits prev (rest pits)))
        ((or (null pits)
             (eq it (first pits))))
      (index-insert self start (first pits))
      (setf start (rest start)))

    (unless m (return self))))

(defun index-trans-reset (self)
  (rplacd self nil))

(defun index-commit (&key (trans *index-trans*))
  ;; Clears changes made in TRANS
  (when trans
    (index-trans-reset trans)))

(defun index-rollback (&key (trans *index-trans*))
  ;; Rolls back and clears changes made in TRANS
  (when trans
    (dolist (ch (nreverse (rest trans)))
      (ecase (ch-op ch)
        (:add
         (index-remove (ch-index ch) (ch-key ch)
                       :it (ch-it ch)
                       :trans nil))
        (:remove
         (index-add (ch-index ch) (ch-it ch)
                    :key (ch-key ch)
                    :trans nil))))
    (index-trans-reset trans)))

(defmethod compare ((x idx) y)
  (compare (index-first x) (index-first y)))
