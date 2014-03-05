;;;; util.lisp

(defpackage site-compiler.util
  (:use #:cl)
  (:export #:subfiles
           #:link-emb-name
           #:print-hash-table))

(in-package #:site-compiler.util)

(defun subfiles (base defaults)
  (let ((n (length base)))
    (mapcar
     (lambda (path)
       (make-pathname :directory (cons :relative (nthcdr n (pathname-directory path)))
                      :defaults path))
     (directory (make-pathname :directory (append base '(:wild-inferiors))
                               :defaults defaults)))))

(defun link-emb-name (name)
  (format nil "link:~a" name))

(defvar *ht-print-depth* 0)

(defun print-hash-entry (key value)
  (format t "~&~v,0T~s => " *ht-print-depth* key)
  (print-hash-table value)
  (fresh-line))

(defgeneric print-hash-table (object))

(defmethod print-hash-table ((object hash-table))
  (princ "{")
  (let ((*ht-print-depth* (+ 2 *ht-print-depth*)))
    (maphash #'print-hash-entry object))
  (format t "~v,0T}" *ht-print-depth*))

(defmethod print-hash-table (object)
  (prin1 object))
