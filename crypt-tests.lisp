(defpackage cl4l-crypt-tests
  (:use cl cl4l-crypt cl4l-test))

(in-package cl4l-crypt-tests)

(define-test (:crypt :round-trip)
  (let ((iv (make-iv :aes))
        (msg "message"))
    (flet ((cipher () (make-crypt "secret" iv)))
      (assert (string= msg
                       (decrypt (cipher)
                                (encrypt (cipher) msg)))))))
