;;; Choose letters based on almost complete randomness

(in-package #:cloned-natural-language)

(let* ((vocals '(#\a #\e #\i #\o #\u #\ă #\â #\î))
       (consonants (set-difference *alphabet* vocals)))
  (defun create-word-from-randomness ()
    "Documentation for create-word with parameters "
    (let ((len (+ 2 (random 8))))
      (format nil "~{~a~}"
              (loop for c below len
                    collect (if (/= 0 (mod c 2))
                                (rand-elt consonants)
                                (rand-elt vocals)))))))
