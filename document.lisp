;;;; document.lisp

(defpackage pochopedia2.document
  (:use #:cl #:pochopedia2.config)
  (:import-from #:pochopedia2.util
                #:subfiles
                #:link-emb-name)
  (:import-from #:alexandria
                #:when-let
                #:ensure-list
                #:hash-table-keys
                #:hash-table-values
                #:alist-hash-table
                #:copy-hash-table)
  (:export #:document #:document-contents #:document-schema #:document-name
           #:schema #:schema-name #:schema-template #:schema-keys
           #:schema-includes #:schema-indexed #:schema-links
           #:key-definition #:key-name #:key-link-p #:key-list-p
           #:key-index-p #:key-foreign #:key-reverse #:key-complex-p
           #:load-schema #:load-document
           #:document-pathnames #:schema-pathnames))

(in-package #:pochopedia2.document)

(defclass document ()
  ((contents :type hash-table
             :initarg :contents
             :reader document-contents)
   (schema :type hash-table
           :initarg :schema
           :reader document-schema)
   (name :type string
         :initarg :name
         :reader document-name)))

(defclass schema ()
  ((name :type string
         :initarg :name
         :reader schema-name)
   (template :type string
             :initarg :template
             :reader schema-template)
   (keys :type hash-table
         :initarg :keys
         :reader schema-keys)
   (direct-keys :type hash-table
                :initarg :direct-keys
                :reader schema-direct-keys)
   (includes :type list
             :initarg :includes
             :reader schema-includes)
   (indexed :type hash-table
            :initarg :indexed
            :reader schema-indexed)
   #+nil(links :type hash-table
          :initarg :links
          :reader schema-links)))

(defmethod print-object ((object schema) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (schema-name object) stream)))

(defun pprint-schema (*standard-output* schema)
  (with-slots (name keys includes indexed links) schema
    (pprint-logical-block (*standard-output* schema :prefix "#Schema<" :suffix ">")
      (pprint-newline :miser)
      (write-string name)
      (pprint-newline :fill)
      (write-string "keys: ")
      (write keys)
      (pprint-newline :fill)
      (write-string "includes: ")
      (write includes)
      (pprint-newline :fill)
      (write-string "indexed: ")
      (write indexed)
      (pprint-newline :fill)
      (write-string "links: ")
      (write links))))

(defclass key-definition ()
  ((name :type string
         :initarg :name
         :reader key-name)
   (link :type boolean
         :initarg :link
         :initform nil
         :reader key-link-p)
   (list :type boolean
         :initarg :list
         :initform nil
         :reader key-list-p)
   (index :type boolean
          :initarg :index
          :initform nil
          :reader key-index-p)
   (foreign :type (or string null)
            :initarg :foreign
            :initform nil
            :reader key-foreign)
   (reverse :type (or cons null)
            :initarg :reverse
            :initform nil
            :reader key-reverse)))

(defmethod print-object ((object key-definition) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (key-name object) stream)))

(defun make-key-definition (key-name key-ht)
  (if key-ht
      (make-instance 'key-definition
                     :name key-name
                     :link (gethash "link" key-ht)
                     :list (and (or (gethash "list" key-ht)
                                    (gethash "reverse" key-ht))
                                t)
                     :index (gethash "index" key-ht)
                     :foreign (gethash "foreign" key-ht)
                     :reverse (when-let ((rev (gethash "reverse" key-ht)))
                                (cons (gethash "schema" rev)
                                      (gethash "key" rev))))
      (make-instance 'key-definition :name key-name)))

(defun key-complex-p (key)
  (or (key-foreign key) (key-reverse key)))

;;;TODO: convert hash-tables to classes

(defun file-to-name (filename)
  (princ-to-string (make-pathname :type nil :defaults filename)))

(defun name-to-url (name)
  (concatenate 'string *entity-base-url* name ".html"))

(defun calc-schema-indexed (schema-name includes keys)
  "=> index hash-table
Collects all indexed keys and merges them with indexed keys from included schemas.
The returned hash-table has the indexed keys as keys and a list of schemas
in which they are indexed as values."
  (let ((supers (mapcar #'load-schema includes))
        (indexed (alist-hash-table
                  (mapcan (lambda (key)
                            (when (key-index-p key)
                              (list (list (key-name key) schema-name))))
                          (hash-table-values keys))
                  :test #'equal)))
    (dolist (super supers)
      (maphash (lambda (key sma)
                 (setf (gethash key indexed)
                       (append sma (gethash key indexed))))
               (schema-indexed super)))
    indexed))

(defun calc-schema-links (schema keys)
  "=> link hash-table"
  (let ((links (alist-hash-table
                (mapcan (lambda (name)
                          (let* ((desc (gethash name (gethash "keys" schema)))
                                 (target (and desc (gethash "link" desc))))
                            (when target
                              (list (cons name target)))))
                        (hash-table-keys keys))
                :test #'equal)))
    links))

(defun calc-schema-keys (schema)
  (let ((keys (gethash "keys" schema)))
    (alist-hash-table
     (mapcar #'cons
             (hash-table-keys keys)
             (mapcar #'make-key-definition (hash-table-keys keys) (hash-table-values keys)))
     :test #'equal)))

(defun merge-keys (keys includes)
  (let ((merged (copy-hash-table keys :test #'equal)))
    (dolist (super (mapcar #'load-schema includes))
      (dolist  (s-key (hash-table-values (schema-keys super)))
        (unless (gethash (key-name s-key) merged)
          (setf (gethash (key-name s-key) merged) s-key))))
    merged))

(defun load-schema (filename)
  "=> a schema hash-table
Loads a schema file and calculates :name and :indexed etc.."
  (let* ((schema-path (merge-pathnames filename *schema-dir*))
         (schema (cl-yy:yaml-simple-load (alexandria:read-file-into-string schema-path)))
         (name (file-to-name filename))
         (includes (ensure-list (gethash "include" schema)))
         (direct-keys (calc-schema-keys schema))
         (merged-keys (merge-keys direct-keys includes))
         (cl-emb:*case-sensitivity* t))
    (cl-emb:register-emb (link-emb-name name) (gethash "link" schema "<% @var :name %>"))
    (make-instance 'schema
                   :name name
                   :template (gethash "template" schema)
                   :direct-keys direct-keys
                   :keys merged-keys
                   :includes includes
                   :indexed (calc-schema-indexed name includes direct-keys)
                   ;;:links (calc-schema-links schema keys)
                   )))

(defun load-document (filename)
  (let* ((yaml-text (alexandria:read-file-into-string (merge-pathnames filename *data-dir*)))
         (contents (cl-yy:yaml-simple-load yaml-text))
         (schema (load-schema (gethash "schema" contents "")))
         (name (file-to-name filename))
         (url (name-to-url name))
         (doc (make-instance 'document :contents contents :schema schema :name name)))
    (setf (gethash ":name" contents) name)
    (setf (gethash ":url" contents) url)
    (setf (gethash ":link" contents)
          (format nil "<a href=\"~a\">~a</a>" url
                  (cl-emb:execute-emb (link-emb-name (schema-name schema)) :env doc)))
    doc))

(defun document-pathnames ()
  (subfiles (pathname-directory *data-dir*) "*.yaml"))

(defun schema-pathnames ()
  (subfiles (pathname-directory *schema-dir*) "*.yaml"))

(defun key-link-p* (key schema)
  (when (gethash key (gethash :links schema))
    t))
