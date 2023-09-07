;;;; package.lisp

(defpackage #:cloned-natural-language
  (:use #:cl #:drakma #:cl-ppcre)
  (:export :generate-wordfile-from-url))
