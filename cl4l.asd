(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:cl-cont)
  :serial t
  :components ((:file "cl4l")
               (:file "utils")
               (:file "index")
               (:file "test")
               (:file "iter")
               (:file "critter")
               (:file "table")
               (:file "memoize")
               (:file "index-tests")
               (:file "utils-tests")))
