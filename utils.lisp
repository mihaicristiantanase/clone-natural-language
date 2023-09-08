;;; Bag of utility funcitons

(in-package #:cloned-natural-language)

(defun print-freq-dictionary (freq-dict &optional (level 0))
  "Documentation for print-freq-dictionary with parameters freq-dict"
  (loop for k being the hash-key of freq-dict
        for v = (gethash k freq-dict) do
          (format t "~&~{~a~}~a: ~a~%"
                  (loop for x below level collect (if (= 0 (mod x 3)) "⏐" " "))
                  k (car v))
          (print-freq-dictionary (cdr v) (+ 3 level))))

(defun rand-elt (lst)
  "Documentation for rand-elt with parameters lst"
  (nth (random (length lst)) lst))
