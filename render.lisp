;;;; render.lisp

(defpackage #:site-compiler.render
  (:use #:cl)
  (:import-from #:site-compiler.document
                #:document-contents)
  (:import-from #:djula
                #:render-template* #:compile-template* #:add-template-directory)
  ;;(:export #:render-document)
  )

(in-package #:site-compiler.render)

