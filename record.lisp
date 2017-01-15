(defpackage cl4l-record
  (:export record-clone)
  (:use cl))

(in-package cl4l-record)

(defgeneric record-clone (self)
  (:method (self)
    self)
  (:method ((self list))
    (copy-list self)))
