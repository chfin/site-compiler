;;;; site-compiler.asd

(asdf:defsystem #:site-compiler
  :serial t
  :description "A static site compiler from yaml files using templates and schemas"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:ningle
               #:cl-yaclyaml
               #:cl-emb)
  :components ((:file "util")
               (:file "config")
               (:file "document")
               (:file "index")
               (:file "site-compiler")))

