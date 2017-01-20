(defpackage cl4l-channel
  (:export make-channel channel-close channel-get
           channel-length channel-put)
  (:shadowing-import-from bordeaux-threads
                          make-lock with-lock-held)
  (:use cl cl4l-fifo cl4l-semaphore))

(in-package cl4l-channel)

(defstruct (channel (:conc-name ch-)
                    (:constructor make-ch))
  (buffer (make-fifo))
  (gets)
  (lock (make-lock))
  (puts))

(defun make-channel (&key (max-length 0))
  "Returns new channel with optional MAX-LENGTH"
  (let ((ch (make-ch)))
    (setf (ch-gets ch) (make-semaphore :count max-length
                                       :lock (ch-lock ch))
          (ch-puts ch) (make-semaphore :lock (ch-lock ch)))
    ch))

(defun channel-get (self)
  "Returns next item from SELF"
  (semaphore-signal (ch-gets self))
  (semaphore-wait (ch-puts self))
  (with-lock-held ((ch-lock self))
    (fifo-pop (ch-buffer self))))

(defun channel-length (self)
  "Returns number of items in SELF"
  (semaphore-count (ch-puts self)))

(defun channel-put (self it)
  "Puts IT into SELF and returns IT"
  (semaphore-wait (ch-gets self))
  (with-lock-held ((ch-lock self))
    (fifo-push (ch-buffer self) it))
  (semaphore-signal (ch-puts self))
  it)
