(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:ironclad)
  :serial t
  :components ((:file "cl4l")
               (:file "utils")
               (:file "event")
               (:file "crypt")
               (:file "iter")
               (:file "index")
               (:file "test")
               (:file "record")
               (:file "table")
               (:file "database")
               (:file "memoize")
               (:file "event-tests")
               (:file "index-tests")
               (:file "iter-tests")
               (:file "table-tests")
               (:file "utils-tests")))
