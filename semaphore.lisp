(defpackage cl4l-semaphore
  (:export make-semaphore semaphore-count semaphore-signal
           semaphore-wait)
  (:shadowing-import-from bordeaux-threads
                          condition-notify condition-wait
                          make-condition-variable make-lock
                          with-lock-held)
  (:use cl))

(in-package cl4l-semaphore)

(defstruct (semaphore (:conc-name sem-)
                      (:constructor make-semaphore))
  (lock (make-lock))
  (wait (make-condition-variable))
  (count 0))

(defun semaphore-signal (self)
  "Increments SELF and wakes up waiting thread if any"
  (with-lock-held ((sem-lock self))
    (when (<= (incf (sem-count self)) 0)
      (condition-notify (sem-wait self)))))

(defun semaphore-wait (self)
  "Decrements SELF and blocks thread if negative"
  (with-lock-held ((sem-lock self))
    (when (< (decf (sem-count self)) 0)
      (condition-wait (sem-wait self) (sem-lock self)))))

(defun semaphore-count (self)
  "Returns count (signals - waits) of SELF"
  (with-lock-held ((sem-lock self))
    (sem-count self)))
