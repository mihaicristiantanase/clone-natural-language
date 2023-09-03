;;;; cloned-natural-language.asd

(asdf:defsystem #:cloned-natural-language
  :description "cloned-natural-language"
  :author "Mihai Cristian Tănase"
  :license  "Mihai Cristian Tănase"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl)
  :components ((:file "package")
               (:file "cloned-natural-language")))
