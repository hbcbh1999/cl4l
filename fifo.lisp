(defpackage cl4l-fifo
  (:export make-fifo fifo-length fifo-peek fifo-pop fifo-push)
  (:shadowing-import-from cl4l-utils when-let)
  (:use cl))

(in-package cl4l-fifo)

(defstruct (fifo (:conc-name q) (:constructor make-fifo))
  head (length 0) tail)

(defun fifo-length (self)
  (qlength self))

(defun fifo-peek (self)
  "Returns first item from SELF"
  (first (qhead self)))

(defun fifo-pop (self)
  "Removes first item from SELF"
  (when-let (it (pop (qhead self)))
    (unless (qhead self)
      (setf (qtail self) nil))
    (decf (qlength self))
    it))

(defun fifo-push (self it)
  "Adds IT to end of SELF"
  (if (null (qhead self))
      (setf (qtail self) (setf (qhead self) (list it)))
      (setf (rest (qtail self)) (list it)
            (qtail self) (rest (qtail self))))
  (incf (qlength self))
  it)
