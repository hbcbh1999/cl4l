(defpackage cl4l-database
  (:export database database-open database-close
           database-flush define-database make-database)
  (:shadowing-import-from cl4l-utils string! symbol! with-symbols)
  (:use cl cl4l-index cl4l-table))

(in-package cl4l-database)

(defmacro define-database (name &body forms)
  (with-symbols (_cons _self)
    `(progn
       (defstruct (,name (:constructor ,_cons)
                         (:conc-name)
                         (:include database))
         ,@(mapcar #'first forms))
       
       (defun ,(symbol! 'make- name) (&key path)
         (macrolet ((,(symbol! name '-stream) (name)
                      `(database-open ,',_self ,name)))
           (let ((,_self (,_cons :path path)))
             ,@(mapcar (lambda (f)
                         `(setf (,(first f) ,_self) ,(second f)))
                       forms)
             ,_self))))))

(defstruct (database (:conc-name db-) (:constructor make-db))
  path
  streams
  tbls
  idxs)

(defun make-database (&rest args)
  (apply #'make-db args))

(defun database-open (self name)
  (let ((s (open (string! (db-path self) name)
                 :direction :io
                 :if-exists :append
                 :if-does-not-exist :create)))
    (push s (db-streams self))
    s))

(defun database-close (self)
  (dolist (s (db-streams self))
    (close s))
  (setf (db-streams self) nil))

(defun database-flush (self)
  (dolist (s (db-streams self))
    (force-output s)))
