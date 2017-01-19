(defpackage cl4l-channel
  (:export make-channel channel-get channel-length channel-put)
  (:shadowing-import-from bordeaux-threads
                          make-lock with-lock-held)
  (:use cl cl4l-fifo cl4l-semaphore))

(in-package cl4l-channel)

(defstruct (channel (:conc-name ch-)
                    (:constructor make-ch))
  (buffer (make-fifo))
  (gets)
  (lock (make-lock))
  (puts (make-semaphore)))

(defun make-channel (&optional (max-length 0))
  (make-ch :gets (unless (zerop max-length)
                   (make-semaphore :count max-length))))

(defun channel-get (self)
  (semaphore-wait (ch-puts self))
  (with-lock-held ((ch-lock self))
    (let ((it (fifo-pop (ch-buffer self))))
      (when (ch-gets self)
        (semaphore-signal (ch-gets self)))
      it)))

(defun channel-length (self)
  (with-lock-held ((ch-lock self))
    (semaphore-count (ch-puts self))))

(defun channel-put (self it)
  (when (ch-gets self)
    (semaphore-wait (ch-gets self)))
  (with-lock-held ((ch-lock self))
    (fifo-push (ch-buffer self) it)
    (semaphore-signal (ch-puts self))))
