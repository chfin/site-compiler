;;;; render.lisp

(defpackage #:site-compiler.render
  (:use #:cl)
  (:import-from #:site-compiler.util
                #:link-emb-name)
  (:export #:*inline-template-engine*
           #:register-inline-template))

(in-package #:site-compiler.render)

(defvar *inline-template-engine* :djula
  "contains the currently used inline template engine (`:djula` or `:cl-emb`)")

(defvar *djula-inline-templates* (make-hash-table :test #'equal)
  "keeps the registered inline templates for djula")

(defvar +default-link-cl-emb+ "<% @var _name %>")
(defvar +default-link-djula+ "{{ _name }}")

(defun register-inline-template (name template)
  "compiles and saves `template` under `name`"
  (case *inline-template-engine*
    (:cl-emb
     (let ((cl-emb:*case-sensitivity* t))
       (cl-emb:register-emb (link-emb-name name) (or template *default-link-cl-emb*))))
    (:djula
     (setf (gethash name *djula-inline-templates*) (djula::compile-string template)))))
