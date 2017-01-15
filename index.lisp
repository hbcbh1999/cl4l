(defpackage cl4l-index
  (:export do-index
           index index-add index-clone index-commit index-delete
           index-diff index-dump index-find index-first
           index-insert
           index-iter index-join index-key
           index-last index-length index-match index-merge
           index-on-add index-on-remove
           index-prev index-read index-remove index-rollback
           index-slurp
           index-subscribe index-write make-index make-index-trans
           with-index-trans *index-trans*)
  (:shadowing-import-from cl4l-utils compare key-gen when-let
                          with-symbols)
  (:use cl cl4l-event cl4l-iter))

(in-package cl4l-index)

;; Default trans
(defvar *index-trans* nil)

(defmacro do-index ((expr rec) &body body)
  `(do-iter (,expr ,rec)
     ,@body))

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

(defstruct (index (:conc-name idx-) (:constructor make-idx)) 
  head
  key-gen
  (length 0)
  (on-add (make-event))
  (on-remove (make-event))
  stream
  tail
  (unique? t))

(defstruct (ch)
  op idx key rec)

(defun make-index (&key key
                        key-gen
                        (head (list nil))
                        stream
                        tail
                        (length 0)
                        (unique? t))
  ;; Returns a new index from ARGS
  (make-idx :key-gen (or key-gen (key-gen key))
            :head head
            :stream stream
            :tail (or tail (last head))
            :length length
            :unique? unique?))

(defun make-index-trans ()
  (list nil))

(defun index (key &rest recs)
  ;; Returns a new index with KEY, initialized from RECS
  (let ((srecs (and recs (stable-sort recs 
                                    (lambda (x y) 
                                      (= (compare x y) -1)) 
                                    :key key))))
    (make-index :key key
                :head (cons nil srecs) 
                :length (length srecs))))

(defun index-clone (self)
  ;; Returns clone of SELF
  (let ((recs (copy-list (idx-head self))))
    (make-index :key-gen (idx-key-gen self) 
		:head recs 
		:length (idx-length self)
                :unique? (idx-unique? self))))

(defun index-key (self rec)
  ;; Returns key for REC in SELF
  (funcall (idx-key-gen self) rec))

(defun (setf index-key) (key self)
  ;; Sets KEY in SELF
  (setf (idx-key-gen self) (key-gen key)))

(defun index-last (self)
  ;; Returns the last record from SELF
  (idx-tail self))

(defun index-length (self)
  ;; Returns the length of SELF
  (idx-length self))

(defun index-prev (self key &key rec start)
  ;; Returns the previous record in SELF matching KEY/REC,
  ;; from START excl.
  (unless start (setf start (idx-head self)))
  (if (null (rest start))
      (values start nil 0)
      (let* ((lrec (idx-tail self))
	     (lrec-cmp (compare key 
                               (index-key self (first lrec)))))
	(if  (> lrec-cmp 0)
	     (values lrec (zerop lrec-cmp) (idx-length self))
	     (do ((recs start (rest recs))
		  (pos 0 (1+ pos)))
		 ((null (rest recs)) 
		  (values (idx-tail self) nil pos))
	       (let ((cmp (compare key 
                                   (index-key 
                                    self (second recs)))))
                 (when (and (zerop cmp)
                            (not (idx-unique? self))
                            rec)
                   (setf cmp (compare rec (second recs))))
		 (when (< cmp 1)
		   (return (values recs (zerop cmp) pos)))))))))

(defun index-first (self &key key rec)
  ;; Returns all records in SELF, optionally from KEY/REC incl.
  (rest (if key (index-prev self key :rec rec) (idx-head self))))

(defun index-find (self key &key rec start)
  ;; Returns record with KEY/REC in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found?) 
      (index-prev self key :rec rec :start start)
    (when found? (first (rest prev)))))

(defun index-pos (self key rec &key start)
  ;; Returns the position of KEY/REC in SELF, from START excl.;
  ;; or NIL if not found.
  (multiple-value-bind (prev found? pos) 
      (index-prev self key :rec rec :start start)
    (declare (ignore prev))
    (when found? pos)))

(defun index-insert (self prev rec)
  ;; Inserts REC after PREV in SELF and returns REC
  (let* ((recs (push rec (rest prev))))
    (when (eq prev (idx-tail self))
      (setf (idx-tail self) recs))
    (incf (idx-length self))
    rec))

(defun index-add (self rec &key (key (index-key self rec))
                               start
                               (trans *index-trans*))
  ;; Adds REC to SELF after START and returns REC
  (multiple-value-bind (prev found?)
      (index-prev self key :rec rec :start start)
    (unless (and found?
                 (or (idx-unique? self)
                     (eq rec (second prev))))
      (event-publish (idx-on-add self) rec)
      (if trans
        (push (make-ch :op :add :idx self :key key :rec rec)
              (rest trans))
        (when-let (stream (idx-stream self))
          (index-write self :add rec :stream stream)))
      (index-insert self prev rec))))

(defun index-delete (self prev)
  ;; Deletes record after PREV from SELF returns it
  (when (eq (rest prev) (idx-tail self))
    (setf (idx-tail self) prev))
  (let ((rec (second prev)))
    (pop (rest prev))
    (decf (idx-length self))
    rec))

(defun index-on-add (self)
  (idx-on-add self))

(defun index-iter (self &key (start (idx-head self)))
  (dolist (rec start)
    (iter-yield rec)))

(defun index-on-remove (self)
  (idx-on-remove self))

(defun index-remove (self key &key rec start (trans *index-trans*))
  ;; Removes KEY/REC from SELF after START and returns prev
  (multiple-value-bind (prev found?) 
      (index-prev self key :rec rec :start start)
    (when found?
      (event-publish (idx-on-remove self) rec)

      (if trans
        (push (make-ch :op :remove
                       :idx self
                       :key key :rec (second prev))
              (rest trans))
        (when-let (stream (idx-stream self))
          (index-write self :remove found? :stream stream)))
      (index-delete self prev))))

(defun index-match (self other &key prev-match)
  ;; Returns next matching records from (SELF . OTHER),
  ;; optionally starting from PREV-MATCH.
  (unless prev-match
    (setf prev-match (cons (idx-head self) (idx-head other))))
  (let* ((start (first prev-match)) (prev-irec start))
    (do ((irec (rest start)) 
         (jrec (rest (rest prev-match))))
        ((or (null irec) (null jrec)) nil)
      (let* ((ikey (index-key self (first irec)))
             (jkey (index-key self (first jrec)))
             (cmp (compare ikey jkey)))
        (case cmp
          (-1 
           (setf prev-irec 
                 (index-prev self jkey
                             :rec (first jrec)
                             :start prev-irec))
           (setf irec (rest prev-irec)))
          (1 (setf jrec 
                   (rest (index-prev other ikey
                                     :rec (first irec) 
                                     :start jrec))))
          (t
           (return (cons prev-irec jrec))))))))

(defun index-join (self other)
  ;; Removes all records from SELF that are not found in OTHER and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (prev (idx-head self) (rest (first m)))) (nil)
    (setf m (index-match self other :prev-match m))
    
    (do ((rec (second (first m))))
        ((eq rec (second prev)) nil)
      (index-delete self prev))
    
    (unless m (return self))))

(defun index-diff (self other)
  ;; Removes all records from SELF that are found in OTHER and
  ;; returns SELF.
  (do ((m nil)) (nil)
    (setf m (index-match self other :prev-match m))
    (when m (index-delete self (first m)))
    (unless m (return self))))

(defun index-dump (self &key (stream (idx-stream self)))
  (dolist (rec (index-first self) self)
    (index-write self :add rec :stream stream)))

(defun index-merge (self other)
  ;; Adds all records from OTHER that are not found in SELF and
  ;; returns SELF.
  (do ((m nil)
       (first? t nil)
       (start (idx-head self) (rest (first m)))
       (prev (index-first other) (rest (rest m)))) (nil)
    (setf m (index-match self other :prev-match m))

    (do ((rec (first (rest m)))
         (precs prev (rest precs)))
        ((or (null precs)
             (eq rec (first precs))))
      (index-insert self start (first precs))
      (setf start (rest start)))

    (unless m (return self))))

(defun index-trans-reset (self)
  (rplacd self nil))

(defun index-commit (&key (trans *index-trans*))
  ;; Clears changes made in TRANS
  (when trans
    (dolist (ch (nreverse (rest trans)))
      (when-let (stream (idx-stream (ch-idx ch)))
        (index-write (ch-idx ch)
                     (ch-op ch)
                     (ch-rec ch)
                     :stream stream)))

    (index-trans-reset trans)))

(defun index-read (self &key (stream (idx-stream self)))
  (when-let (ln (read-line stream nil))
    (let ((form (read-from-string ln)))
      (ecase (first form)
        (:add
         (index-add self (rest form)))
        (:remove
         (index-remove self (rest form)))))
    t))

(defun index-slurp (self &key (stream (idx-stream self)))
  (tagbody
   next
     (when (index-read self :stream stream)
       (go next))))

(defun index-rollback (&key (trans *index-trans*))
  ;; Rolls back and clears changes made in TRANS
  (when trans
    (dolist (ch (nreverse (rest trans)))
      (ecase (ch-op ch)
        (:add
         (index-remove (ch-idx ch) (ch-key ch)
                       :rec (ch-rec ch)
                       :trans nil))
        (:remove
         (index-add (ch-idx ch) (ch-rec ch)
                    :key (ch-key ch)
                    :trans nil))))
    (index-trans-reset trans)))

(defgeneric index-subscribe (self sub)
  (:method (self (sub index))
    (event-subscribe (idx-on-add self)
                     (lambda (rec)
                       (index-add sub rec)))
    
    (event-subscribe (idx-on-remove self)
                     (lambda (rec)
                       (index-remove sub (index-key sub rec))))
    sub))

(defun index-write (self op rec &key (stream (idx-stream self)))
  (write (ecase op
           (:add (cons op rec))
           (:remove (cons op (index-key self rec))))
         :stream stream)
  (terpri stream))

(defmethod compare ((x index) y)
  (compare (index-first x) (index-first y)))
