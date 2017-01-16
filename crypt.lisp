(defpackage cl4l-crypt
  (:export make-init-vector)
  (:shadowing-import-from ironclad block-length make-prng
                          random-data)
  (:use cl))

(in-package cl4l-crypt)

(defvar *prng* (make-prng :fortuna))

(defun make-init-vector (cipher)
  (random-data (block-length cipher) *prng*))
