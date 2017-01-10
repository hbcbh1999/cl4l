(defpackage cl4l-utils-tests
  (:use cl cl4l-test cl4l-utils))

(in-package cl4l-utils-tests)

(define-test (:defer)
  (assert (string= "hello world"
                   (with-output-to-string (out)
                     (with-defer (outer)
                       (with-defer ()
                         (defer (format out "hello"))
                         (defer-outer (format out "world")))
                       (format out " "))))))
