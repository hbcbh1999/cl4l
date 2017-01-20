(defpackage cl4l-channel-tests
  (:shadowing-import-from bordeaux-threads
                          join-thread make-thread)
  (:use cl cl4l-channel cl4l-test))

(in-package cl4l-channel-tests)

(defparameter test-max 10000)

(defun producer (ch)
  (dotimes (i test-max)
    (channel-put ch i)))

(defun consumer (ch)
  (dotimes (i test-max)
    (assert (= i (channel-get ch)))))

(define-test (:channel)
  ;; Unbuffered channel to force rendezvous
  (let ((ch (make-channel)))
    (make-thread (lambda () (consumer ch)))
    (join-thread (make-thread (lambda () (producer ch))))))
