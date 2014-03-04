;;;; config.lisp

(defpackage pochopedia2.config
  (:use #:cl)
  (:export #:*data-dir*
           #:*static-dir*
           #:*template-dir*
           #:*schema-dir*
           #:*entity-base-url*))

(in-package #:pochopedia2.config)

(defparameter *data-dir* "/home/chfin/lisp/projects/pochopedia2/data/default.yaml")
(defparameter *static-dir* "/home/chfin/lisp/projects/pochopedia2/static/default.html")
(defparameter *template-dir* "/home/chfin/lisp/projects/pochopedia2/templates/default.tmpl")
(defparameter *schema-dir* "/home/chfin/lisp/projects/pochopedia2/schema/default.yaml")
(defparameter *entity-base-url* "")
