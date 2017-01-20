(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:bordeaux-threads :flexi-streams :ironclad)
  :serial t
  :components ((:file "cl4l")
               (:file "utils")
               (:file "crypt")
               (:file "event")
               (:file "iter")
               (:file "index")
               (:file "test")
               (:file "record")
               (:file "table")
               (:file "database")
               (:file "fifo")
               (:file "semaphore")
               (:file "chan")
               (:file "memoize")
               (:file "chan-tests")
               (:file "crypt-tests")
               (:file "event-tests")
               (:file "index-tests")
               (:file "iter-tests")
               (:file "memoize-tests")
               (:file "table-tests")
               (:file "utils-tests")))
