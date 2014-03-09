;;;; test/site-compiler-test.asd

(asdf:defsystem #:site-compiler-test
  :serial t
  :description "Tests for site-compiler."
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:site-compiler)
  :components ((:file "test")))
