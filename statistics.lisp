;;; Choose letters based on statistics

(in-package #:cloned-natural-language)

(defun load-percentages-from-corpus (fname)
  "Documentation for load-percentages-from-corpus with parameters fname"
  (with-open-file (f fname)
    (let* ((words (loop for line = (read-line f nil nil nil) while line
                        collect line))
           (nwords (length words)))
      (let ((chars-per-level
              (loop for level from 0 below (apply #'max (mapcar #'length words))
                    collect (loop for w in words
                                  collect (when (< level (length w)) (aref w level))))))
        (loop for level-chars in chars-per-level
              collect (loop for ch in (remove-duplicates level-chars)
                            when ch
                              collect (cons ch (round (* (count ch level-chars) 100) nwords))))))))

(defparameter *percentages-per-length*
  (load-percentages-from-corpus "română.txt"))

(defun choose-letter ()
  "Randomly choose a letter based on *percentages* alist"
  (let ((r (random 100)))
    (let ((running-sum 0))
      (loop for (ch . perc) in *percentages* do
        (if (< r (+ running-sum perc))
            (return-from choose-letter ch)
            (incf running-sum perc))))))

(defun create-word-from-statistics ()
  "Create a word using statistics"
  (format nil "~{~a~}"
          (loop for level from 0
                for *percentages* = (when (< level (length *percentages-per-length*))
                                      (nth level *percentages-per-length*))
                for ch = (choose-letter)
                while ch
                collect ch)))
