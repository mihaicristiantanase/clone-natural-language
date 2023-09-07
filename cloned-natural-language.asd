;;;; cloned-natural-language.asd

(asdf:defsystem #:cloned-natural-language
  :description "cloned-natural-language"
  :author "Mihai Cristian Tănase"
  :license  "Mihai Cristian Tănase"
  :version "0.0.1"
  :serial t
  :depends-on (:drakma :cl-ppcre)
  :components ((:file "package")
               (:file "config")
               (:file "randomness")
               (:file "statistics")
               (:file "n-gram")
               (:file "generate-inputs")
               (:file "cloned-natural-language")
               ;; tests
               (:file "statistics-tests")))
