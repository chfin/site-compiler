;;;; test/test.lisp

(defpackage site-compiler.test
  (:use #:cl #:site-compiler)
  (:export #:run-test))

(in-package #:site-compiler.test)

(defun rel-path (path)
  (merge-pathnames path (asdf:system-source-directory :site-compiler-test)))

(defun run-test ()
  (let ((*data-dir* (rel-path "data/default.yaml"))
        (*base-url* (format nil "file://~a" (rel-path "site/"))))
    (let ((*template-dir* (rel-path "templates/default.tmpl"))
          (*site-dir* (rel-path "site/default.html"))
          (*schema-dir* (rel-path "schema/default.yaml")))
      (write-line "Test djula engine!")
      (write-line "Testing with cold chaches:")
      (time (compile-all :clear-caches t))
      (write-line "Testing with hot chaches:")
      (time (compile-all)))
    t))
