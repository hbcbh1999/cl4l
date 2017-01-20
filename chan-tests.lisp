(defpackage cl4l-chan-tests
  (:shadowing-import-from bordeaux-threads
                          join-thread make-thread)
  (:use cl cl4l-chan cl4l-test))

(in-package cl4l-chan-tests)

(defparameter test-max 10000)

(defun producer (ch)
  (dotimes (i test-max)
    (chan-put ch i)))

(defun consumer (ch)
  (dotimes (i test-max)
    (assert (= i (chan-get ch)))))

(define-test (:chan)
  ;; Unbuffered chan to force rendezvous
  (let ((ch (make-chan)))
    (make-thread (lambda () (consumer ch)))
    (join-thread (make-thread (lambda () (producer ch))))))
