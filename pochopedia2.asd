;;;; pochopedia2.asd

(asdf:defsystem #:pochopedia2
  :serial t
  :description "Describe pochopedia2 here"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:ningle
               #:cl-yaclyaml
               #:cl-emb)
  :components ((:file "util")
               (:file "config")
               (:file "document")
               (:file "index")
               (:file "pochopedia2")))

