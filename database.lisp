(defpackage cl4l-database
  (:export database)
  (:use cl cl4l-index cl4l-table))

(in-package cl4l-database)

(defstruct (database (:conc-name db-) (:constructor make-db)))
