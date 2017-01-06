(defpackage cl4l-field
  (:export make-field
           field-val
           field-tests)
  (:use common-lisp))

(in-package cl4l-field)

(defstruct (field (:conc-name fld-) (:constructor make-fld)) 
  (name (gensym))
  (get nil)
  (set nil))

(defun make-field (&rest args)
  (apply #'make-fld args))

(defun field-val (self &rest args)
  (assert (fld-get self))
  (apply (fld-get self) args))

(defun (setf field-val) (val self &rest args)
  (assert (fld-set self))
  (apply (if (eq t (fld-set self))
             (fld-get self)
             (fld-set self)) val args))

(defun field-tests ()
  (let* ((x 0)
         (xf (make-field :get (lambda () x)
                         :set (lambda (v) (incf x v)))))
    (setf (field-val xf) 21)
    (assert (= 21 x))
    (setf (field-val xf) 21)
    (assert (= 42 x))))
