(asdf:defsystem :cl-spice
  :author "Ben Diedrich"
  :license "MIT"
  :description "Common Lisp CFFI interface to JPL's SPICE library"
  :depends-on ("cffi")
  :serial t
  :components
  ((:file "spice")))
