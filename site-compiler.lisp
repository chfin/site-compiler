;;;; pochopedia2.lisp

(defpackage #:site-compiler
  (:nicknames #:pp2)
  (:use #:cl #:site-compiler.config #:site-compiler.document)
  (:import-from #:site-compiler.index
                #:index-get
                #:create-index)
  (:import-from #:alexandria
                #:hash-table-values
                #:when-let)
  (:import-from #:site-compiler.util
                #:print-hash-table)
  (:import-from #:cl-markdown
                #:markdown)
  (:export #:compile-yaml
           #:create-index
           #:compile-all
           #:*data-dir*
           #:*schema-dir*
           #:*template-dir*
           #:*site-dir*
           #:*base-url*))

(in-package #:site-compiler)

;;; use (file-write-date pathname) for compilation

(defun wrap-this (item)
  "=> `'(:|this| item)`"
  (list :|this| item))

(defclass lazy-document ()
  ((name :type string
         :initarg :name
         :reader lazy-doc-name)))

(defun load-lazy-doc (lazy-doc)
  (let ((doc (load-document (lazy-doc-name lazy-doc))))
    (resolve-document doc)
    doc))

(defmethod cl-emb:getf* ((doc document) key &optional default)  
  (gethash (princ-to-string key) (document-contents doc) default))

(defmethod cl-emb:getf* :around (thing key &optional default)
  (let ((result (call-next-method)))
    (if (typep result 'lazy-document)
        (load-lazy-doc result)
        result)))

(defun resolve-link (name)
  (gethash ":link" (document-contents (load-document name))))

(defun process-markdown (text)
  (print "Processing markdown")
  (nth-value 1 (markdown text :stream nil)))

(defun resolve-document (document)
  "modifies the document to resolve indirect keys"
  (dolist (key (hash-table-values (schema-keys (document-schema document))))
    (let ((key-val (gethash (key-name key) (document-contents document))))
     (when (key-complex-p key)
       (when-let (rev (key-reverse key)) ;;determine reverse references
         (setf key-val (index-get (car rev) (cdr rev)
                                  (document-name document))))
       (flet ((resolve-id (name)
                (if (key-link-p key)
                    (resolve-link name)
                    (make-instance 'lazy-document :name name))))
         (setf key-val
               (if (key-list-p key) ;;resolve references
                   (mapcar #'resolve-id key-val)
                   (resolve-id key-val)))))
     (when (key-markdown-p key) ;;process markdown
       (setf key-val
             (if (key-list-p key)
                 (mapcar #'process-markdown key-val)
                 (process-markdown key-val))))
     (setf (gethash (key-name key) (document-contents document))
           (if (key-list-p key) ;;wrap, if list
               (mapcar #'wrap-this key-val)
               key-val)))))

(defun compile-yaml (pathname)
  (let* ((document (load-document pathname))
         (template (schema-template (document-schema document))))
    (when template
      (resolve-document document)
      (let ((tp-path (merge-pathnames template *template-dir*))
            (cl-emb:*case-sensitivity* t))
        (alexandria:write-string-into-file
         (cl-emb:execute-emb tp-path :env document)
         (ensure-directories-exist (merge-pathnames (document-name document) *site-dir*))
         :if-exists :supersede))
      nil)))

(defun docs-to-compile ()
  (document-pathnames))

(defun compile-all ()
  (create-index)
  (let ((docs (docs-to-compile)))
    (mapcar #'compile-yaml docs))
  nil)
