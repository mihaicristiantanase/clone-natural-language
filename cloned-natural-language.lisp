;;;; cloned-natural-language.lisp

(in-package #:cloned-natural-language)

(defparameter *percentages* '()
  "An alist with characters and percentages")

(let ((lang-to-voices '((:ro . "Ioana")
                        (:en . "Alex"))))
  (defun get-voice (lang)
    "Documentation for get-voice with parameters lang"
    (cdr (assoc lang lang-to-voices))))

(defun say (text &key (lang :ro))
  "Documentation for say with parameters text"
  (let ((cmd (concatenate 'string
                          "say"
                          " --voice " (get-voice lang)
                          " " text)))
    (format t "$ ~a~%" cmd)
    (uiop:run-program cmd)))
