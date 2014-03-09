;;;; site-compiler.asd

(asdf:defsystem #:site-compiler
  :serial t
  :description "A static site compiler from yaml files using templates and schemas"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:cl-yaclyaml
               #:cl-emb
               #:alexandria
               #:cl-markdown)
  :components ((:file "util")
               (:file "config")
               (:file "document")
               (:file "index")
               (:file "site-compiler")))

