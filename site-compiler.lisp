;;;; site-compiler.lisp

(defpackage #:site-compiler
  (:nicknames #:pp2)
  (:use #:cl #:site-compiler.config #:site-compiler.document)
  (:import-from #:site-compiler.render
                #:*inline-template-engine*)
  (:import-from #:site-compiler.index
                #:index-get
                #:create-index)
  (:import-from #:alexandria
                #:hash-table-values
                #:copy-hash-table
                #:when-let)
  (:import-from #:site-compiler.util
                #:print-hash-table)
  (:import-from #:cl-markdown
                #:markdown)
  (:export #:compile-yaml
           #:preview-yaml
           #:create-index
           #:compile-all
           #:*data-dir*
           #:*schema-dir*
           #:*template-dir*
           #:*site-dir*
           #:*base-url*))

(in-package #:site-compiler)

;;; use (file-write-date pathname) for caching

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

(defmethod access:do-access ((o document) k &key type test key skip-call?)
  (declare (ignore type test key skip-call?))
  (gethash (string-downcase (princ-to-string k)) (document-contents o)))

(defmethod access:do-access :around (o k &key type test key skip-call?)
  (let ((result (call-next-method)))
    (if (typep result 'lazy-document)
        (load-lazy-doc result)
        result)))

(defun resolve-link (name)
  (gethash "_link" (document-contents (load-document name))))

(defun process-markdown (text)
  ;;(print "Processing markdown")
  (nth-value 1 (markdown text :stream nil)))

(defun resolve-document (document)
  "modifies the document to resolve indirect keys"
  (setf (document-contents document)
        (copy-hash-table (document-raw-contents document)))
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
         (when key-val
           (setf key-val
                 (if (key-list-p key) ;;resolve references
                     (mapcar #'resolve-id key-val)
                     (resolve-id key-val))))))
     (when (key-markdown-p key) ;;process markdown
       (setf key-val
             (if (key-list-p key)
                 (mapcar #'process-markdown key-val)
                 (process-markdown key-val))))
     (setf (gethash (key-name key) (document-contents document))
           key-val))))

(defun doc-to-var-plist (document)
  "converts `document`'s contents to a plist with keywords as keys and loaded lazy documents"
  (let ((vars nil))
    (maphash (lambda (k v)
               (push (if (typep v 'lazy-document)
                         (load-lazy-doc v)
                         v)
                     vars)
               (push (alexandria:make-keyword k) vars))
             (document-contents document))
    vars))

(defun djula-render-document (document template stream)
  (let ((djula:*auto-escape* nil))
    (apply #'djula:render-template* template stream (doc-to-var-plist document))))

(defgeneric compile-yaml (name template-engine))

(defmethod compile-yaml (pathname (template-engine (eql :cl-emb)))
  (let* ((*inline-template-engine* :cl-emb)
         (document (load-document pathname))
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

(defmethod compile-yaml (pathname (template-engine (eql :djula)))
  (let* ((*inline-template-engine* :djula)
         (document (load-document pathname))
         (template (schema-template (document-schema document))))
    (when template
      (resolve-document document)
      (let ((djula:*current-store* (make-instance 'djula:file-store)))
        (djula:add-template-directory *template-dir*)
        (with-open-file (stream
                         (ensure-directories-exist
                          (merge-pathnames (document-name document) *site-dir*))
                         :direction :output
                         :if-exists :supersede)
          (djula-render-document document template stream)))
      nil)))

(defun preview-yaml (string doc-name)
  (let* ((document (load-document-from-string string doc-name))
         (template (schema-template (document-schema document))))
    (when template
      (resolve-document document)
      (setf (gethash "_preview" (document-contents document)) t)
      (let ((tp-path (merge-pathnames template *template-dir*))
            (cl-emb:*case-sensitivity* t))
        (cl-emb:execute-emb tp-path :env document)))))

(defun docs-to-compile ()
  (document-pathnames))

(defun compile-all (&key clear-caches (template-engine :djula))
  (when clear-caches
    (clear-caches))
  (create-index)
  (let ((docs (docs-to-compile)))
    (mapcar (lambda (doc) (compile-yaml doc template-engine)) docs))
  nil)
