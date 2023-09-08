;;;; package.lisp

(defpackage #:cloned-natural-language
  (:use #:cl #:drakma #:cl-ppcre)
  (:export
   ;; main
   :generate-wordfile-from-url
   :say
   :create-word

   ;; types of word generators
   :random
   :statistics-level
   :statistics-neighbor
   :n-gram
   :rule
   ))
