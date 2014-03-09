;;;; test/test.lisp

(defpackage site-compiler.test
  (:use #:cl #:site-compiler)
  (:export #:run-test))

(in-package #:site-compiler.test)

(defun rel-path (path)
  (merge-pathnames path (asdf:system-source-directory :site-compiler-test)))

(defun run-test ()
  (let ((*data-dir* (rel-path "data/default.yaml"))
        (*site-dir* (rel-path "site/default.html"))
        (*template-dir* (rel-path "templates/default.tmpl"))
        (*schema-dir* (rel-path "schema/default.yaml"))
        (*base-url* (format nil "file://~a" (rel-path "site/"))))
    (compile-all)
    t))
