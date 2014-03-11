;;;; index.lisp

(defpackage site-compiler.index
  (:use #:cl #:site-compiler.config #:site-compiler.document)
  (:import-from #:site-compiler.util
                #:subfiles)
  (:import-from #:alexandria
                #:hash-table-keys
                #:hash-table-values
                #:when-let
                #:alist-hash-table)
  (:export #:read-schemas
           #:schema-pathnames
           #:create-index
           #:index-get))

(in-package #:site-compiler.index)

(defvar *index*)
(defvar *schemas*)

(defun schema-to-index (schema)
  (let ((keys (schema-keys schema)))
    (when-let ((indices ; find keys with 'index: true'
                (remove-if-not (lambda (key)
                                 (let ((desc (gethash key keys)))
                                   (key-index-p desc)))
                               (hash-table-keys keys))))
      (list (cons (schema-name schema)
                  (alist-hash-table ; create a hash-table with keys -> index table
                   (mapcar (lambda (index)
                             ;; create an empty index table
                             (cons index (make-hash-table :test #'equal)))
                           indices)
                   :test #'equal))))))

(defun read-schemas (pathnames)
  (let ((schemas (make-hash-table)))
    (dolist (schema (mapcar #'load-schema pathnames))
      (setf (gethash (schema-name schema) schemas) schema))
    schemas))

(defun init-index (schema-table)
  (alist-hash-table
   (mapcan #'schema-to-index (hash-table-values schema-table))
   :test #'equal))

(defun fill-index (pathnames index)
  (dolist (doc (mapcar #'load-document pathnames))
    (maphash
     (lambda (i-key targets)
       (dolist (target targets)
         (setf (gethash (gethash i-key (document-raw-contents doc))
                        (gethash i-key (gethash target index)))
               (append
                (gethash (gethash i-key (document-raw-contents doc))
                         (gethash i-key (gethash target index)))
                (list (document-name doc))))))
     (schema-indexed (document-schema doc)))))

(defun create-index ()
  (setf *schemas* (read-schemas (schema-pathnames)))
  (setf *index* (init-index *schemas*))
  (fill-index (document-pathnames) *index*))

(defun index-get (&rest keys)
  (labels ((iget (keys)
             (if keys
                 (gethash (car keys) (iget (cdr keys)))
                 *index*)))
    (iget (reverse keys))))
