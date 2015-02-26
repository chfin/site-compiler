;;;; render.lisp

(defpackage #:site-compiler.render
  (:use #:cl)
  (:import-from #:site-compiler.util
                #:link-emb-name)
  (:export #:*inline-template-engine*
           #:register-inline-template
           #:render-inline-template))

(in-package #:site-compiler.render)

(defvar *inline-template-engine* :djula
  "contains the currently used inline template engine (`:djula` or `:cl-emb`)")

(defvar *djula-inline-templates* (make-hash-table :test #'equal)
  "keeps the registered inline templates for djula")

(defvar +default-link-cl-emb+ "<% @var _name %>")
(defvar +default-link-djula+ "{{ _name }}")

(defun doc-to-keyword-list (document)
  "converts `document`'s contents to a plist with keywords as keys"
  (let ((vars nil))
    (maphash (lambda (k v)
               (push v vars)
               (push (alexandria:make-keyword k) vars))
             (document-contents document))
    vars))

(defun register-inline-template (name template)
  "compiles and saves `template` under `name`"
  (case *inline-template-engine*
    (:cl-emb
     (let ((cl-emb:*case-sensitivity* t))
       (cl-emb:register-emb (link-emb-name name) (or template *default-link-cl-emb*))))
    (:djula
     (setf (gethash name *djula-inline-templates*) (djula::compile-string template)))))

(defun render-inline-template (name document)
  "retrives the saved template using `name` and applies it to `document`"
  (case *inline-template-engine*
    (:cl-emb
     (cl-emb:execute-emb (link-emb-name name) :env document))
    (:djula
     (with-output-to-string (stream)
       (apply #'djula:render-template*
              (gethash name *djula-inline-templates* +default-link-djula+)
              stream
              (doc-to-keyword-list document))))))
