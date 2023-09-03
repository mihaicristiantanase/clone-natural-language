;;; Choose letters based on almost complete randomness

(in-package #:cloned-natural-language)

(defparameter *alphabet*
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i
    #\j #\k #\l #\m #\n #\o #\p #\q #\r
    #\s #\t #\u #\v #\w #\x #\y #\z #\ă
    #\â #\î #\ț #\ș))

(defun rand-elt (lst)
  "Documentation for rand-elt with parameters lst"
  (nth (random (length lst)) lst))

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