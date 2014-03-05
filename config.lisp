;;;; config.lisp

(defpackage site-compiler.config
  (:use #:cl)
  (:export #:*data-dir*
           #:*site-dir*
           #:*template-dir*
           #:*schema-dir*
           #:*base-url*))

(in-package #:site-compiler.config)

(defun rel-path (path)
  (merge-pathnames path (asdf:system-source-directory :site-compiler)))

(defparameter *data-dir* (rel-path "data/default.yaml"))
(defparameter *site-dir* (rel-path "static/default.html"))
(defparameter *template-dir* (rel-path "templates/default.tmpl"))
(defparameter *schema-dir* (rel-path "schema/default.yaml"))
(defparameter *base-url* "")
