(defpackage cl4l-utils
  (:export compare defer do-hash-table let-when
           str! string! symbol!
           with-defer)
  (:import-from cl4l-macro-utils with-gsyms)
  (:use cl))

(in-package cl4l-utils)

(defgeneric compare (x y)
  ;; Returns result of comparing X & Y;
  ;; -1, 0 or 1.
  
  (:method ((x character) y)
    (cond
      ((char< x y) -1)
      ((char> x y) 1)
      (t 0)))

  (:method ((x list) y)
    (do ((xi x (rest xi)) (yi y (rest yi)))
	((and (null xi) (null yi)) 0)
      (let ((cmp (compare (first xi) (first yi))))
	(unless (zerop cmp)
	  (return cmp)))))

  (:method ((x number) y)
    (cond
      ((< x y) -1)
      ((> x y) 1)
      (t 0)))

  (:method ((x string) y)
    (do ((i 0 (1+ i)))
	((= i (min (length x) (length y)))
	 (compare (length x) (length y)))
      (let* ((xc (aref x i))
	     (yc (aref y i))
	     (cmp (compare xc yc)))
	(unless (zerop cmp) (return cmp)))))

  (:method ((x symbol) y)
    (compare (symbol-name x) (symbol-name y))))

(defmacro with-defer ((&optional name) &body body)
  (let ((_name (or name (gensym))))
    `(macrolet ((,(symbol! 'defer- _name) (&body forms)
                  (let ((n ',_name))
                    `(push (lambda () ,@forms) ,n)))
                (defer (&body forms)
                  `(,(symbol! 'defer- ',_name) ,@forms)))
       (let ((,_name))
         (unwind-protect (progn,@body)
           (dolist (fn ,_name)
             (funcall fn)))))))

(defmacro do-hash-table ((tbl key val) &body body)
  (with-gsyms (_found _iter)
    `(with-hash-table-iterator (,_iter ,tbl)
       (tagbody
        start
          (multiple-value-bind (,_found ,key ,val) (,_iter)
            (declare (ignorable ,key ,val))
            (unless ,_found (go end))
            ,@body)
          (go start)
        end))))

(defmacro let-when ((var expr cnd) &body body)
  `(let ((,var ,expr))
     (when ,cnd ,@body)
     ,var))

(defun str! (x)
  (if (stringp x) x (princ-to-string x)))

(defun string! (&rest args)
  (apply #'concatenate 'string (mapcar #'str! args)))

(defun symbol! (&rest args)
  (intern (string-upcase (apply #'string! args))))
