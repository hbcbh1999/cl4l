(asdf:defsystem cl4l
  :description "esoteric CL essentials"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "cl4l")
               (:file "utils")
               (:file "index")
               (:file "test")
               (:file "coro")
               (:file "table")
               (:file "memoize")
               (:file "index-tests")
               (:file "utils-tests")))
