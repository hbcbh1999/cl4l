(defpackage cl4l-crypt-tests
  (:use cl cl4l-crypt cl4l-test))

(in-package cl4l-crypt-tests)

(define-test (:crypt :aes)
  (let* ((iv (make-iv :aes))
         (msg "message"))
    (flet ((aes () (make-aes "secret" iv)))
      (assert (string= msg (decrypt (aes) (encrypt (aes) msg)))))))
