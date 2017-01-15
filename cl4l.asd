(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "cl4l")
               (:file "utils")
               (:file "event")
               (:file "iter")
               (:file "index")
               (:file "test")
               (:file "table")
               (:file "memoize")
               (:file "event-tests")
               (:file "index-tests")
               (:file "iter-tests")
               (:file "table-tests")
               (:file "utils-tests")))
