(defpackage cl4l-utils
  (:export compare defer do-hash-table key-gen let-when
           str! string! symbol!
           when-let with-defer with-symbols)
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

  (:method ((x cons) y)
    (let ((xrest (rest x)))
      (if (consp xrest)
          (do ((xi x (rest xi)) (yi y (rest yi))) (nil)
            (when (null xi) (return (if (null yi) 0 -1)))
            (when (null yi) (return 1))
            (let ((cmp (compare (first xi) (first yi))))
              (unless (zerop cmp)
                (return cmp))))
          (let ((cmp (compare (first x) (first y))))
            (if (and (zerop cmp) xrest)
                (compare xrest (rest y))
                cmp)))))
  
  (:method ((x number) y)
    (cond
      ((< x y) -1)
      ((> x y) 1)
      (t 0)))

  (:method ((x string) y)
    (do ((i 0 (1+ i))) (nil)
      (when (= i (length x)) (return (if (= i (length y)) 0 -1)))
      (when (= i (length y)) (return 1))
      
      (let* ((xc (aref x i))
	     (yc (aref y i))
	     (cmp (compare xc yc)))
	(unless (zerop cmp) (return cmp)))))

  (:method ((x symbol) y)
    (compare (symbol-name x) (symbol-name y))))

(defmacro with-symbols ((&rest vars) &body body)
  `(let (,@(mapcar (lambda (v) `(,v (gensym))) vars))
     ,@body))

(defmacro with-defer (name &body body)
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

(defmacro do-hash-table ((tbl key val &key result) &body body)
  (with-symbols (_found _iter)
    `(with-hash-table-iterator (,_iter ,tbl)
       (tagbody
        start
          (multiple-value-bind (,_found ,key ,val) (,_iter)
            (declare (ignorable ,key ,val))
            (unless ,_found (go end))
            ,@body)
          (go start)
        end)
       ,result)))

(defun key-gen (key)
  (cond
    ((null key) (lambda (it) it))
    ((atom key) (lambda (it) (funcall key it)))
    ((and (consp key)
          (consp (rest key)))
     (lambda (it)
       (mapcar (lambda (k)
                 (if (null k) it (funcall k it)))
               key)))
    ((consp key)
     (lambda (it)
       (cons (funcall (first key) it)
             (funcall (rest key) it))))))

(defmacro when-let ((cnd expr) &body body)
  `(let ((,cnd ,expr))
     (when ,cnd
       ,@body)
     ,cnd))

(defmacro let-when ((cnd var expr) &body body)
  `(when ,cnd
     (let ((,var ,expr))
       ,@body)
     ,var))

(defun str! (x)
  (if (stringp x) x (princ-to-string x)))

(defun string! (&rest args)
  (apply #'concatenate 'string (mapcar #'str! args)))

(defun symbol! (&rest args)
  (intern (string-upcase (apply #'string! args))))
