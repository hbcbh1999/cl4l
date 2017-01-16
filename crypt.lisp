(defpackage cl4l-crypt
  (:export decrypt encrypt make-aes make-iv sha256)
  (:shadowing-import-from ironclad block-length
                          decrypt-in-place
                          digest-sequence
                          encrypt-in-place
                          make-cipher
                          make-prng
                          random-data)
  (:shadowing-import-from flexi-streams
                          octets-to-string
                          string-to-octets)
  (:use cl))

(in-package cl4l-crypt)

(defvar *prng* (make-prng :fortuna))

(defun make-iv (cipher)
  (random-data (block-length cipher) *prng*))

(defmethod sha256 (phrase)
  (digest-sequence :sha256 (string-to-octets phrase)))

(defun make-aes (phrase iv)
  (make-cipher :aes :key (sha256 phrase) 
                    :mode :ctr
                    :initialization-vector iv))

(defun encrypt (cipher msg)
  (let ((bytes (string-to-octets msg)))
    (encrypt-in-place cipher bytes)
    bytes))

(defun decrypt (cipher msg)
  (decrypt-in-place cipher msg)
  (octets-to-string msg))
