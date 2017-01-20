(defpackage cl4l-fifo
  (:export make-fifo fifo-pop fifo-push)
  (:shadowing-import-from cl4l-utils when-let)
  (:use cl))

(in-package cl4l-fifo)

(defstruct (fifo (:conc-name q) (:constructor make-fifo))
  head tail)

(defun fifo-pop (self)
  "Removes first item from SELF"
  (when-let (it (pop (qhead self)))
    (unless (qhead self)
      (setf (qtail self) nil))
    it))

(defun fifo-push (self it)
  "Adds IT to end of SELF"
  (if (null (qhead self))
      (setf (qtail self) (setf (qhead self) (list it)))
      (setf (rest (qtail self)) (list it)
            (qtail self) (rest (qtail self))))
  it)
