;;; Choose letters based on statistics

(in-package #:cloned-natural-language)

(defun load-percentages-per-level-from-corpus (fname)
  "Documentation for load-percentages-per-level-from-corpus with parameters fname"
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

(defun load-percentages-per-neighbor-from-corpus (fname)
  "Documentation for load-percentages-per-neighbor-from-corpus with parameters words"
  (with-open-file (f fname)
    (let ((words (loop for line = (read-line f nil nil nil) while line
                       collect line)))
      ;; load mutli-level dictionary
      (let ((rv (make-hash-table)))
        (loop for word in words
              for current-dict = rv do
                (loop for ch across word do
                  (let ((pd (gethash ch current-dict)))
                    (unless pd
                      (setf pd (cons 0 (make-hash-table)))
                      (setf (gethash ch current-dict) pd))
                    (incf (car pd))
                    (setf current-dict (cdr pd)))))
        ;; convert frequencies to percentages
        (labels ((freq-2-prec (d)
                   (let ((sum (reduce '+ (loop for k being the hash-key of d
                                               collect (car (gethash k d))))))
                     (loop for v being the hash-value of d do
                       (setf (car v) (round (* (car v) 100) sum)))
                     (loop for v being the hash-value of d do
                       (freq-2-prec (cdr v))))))
          (freq-2-prec rv))
        rv))))

(defparameter *percentages-per-level*
  (load-percentages-per-level-from-corpus *word-file*))

(defparameter *percentages-per-neighbor*
  (load-percentages-per-neighbor-from-corpus *word-file*))

(defun choose-letter ()
  "Randomly choose a letter based on *percentages* alist"
  (let ((r (random 100)))
    (let ((running-sum 0))
      (loop for (ch . perc) in *percentages* do
        (if (< r (+ running-sum perc))
            (return-from choose-letter ch)
            (incf running-sum perc))))))

(defun create-word-from-statistics (mode)
  "Create a word using statistics"
  (format nil "~{~a~}"
          (ecase mode
            (level (loop for level from 0
                         for *percentages* = (when (< level (length *percentages-per-level*))
                                               (nth level *percentages-per-level*))
                         for ch = (choose-letter)
                         while ch
                         collect ch))
            (neighbor (let ((pn *percentages-per-neighbor*))
                        (loop for *percentages* = (loop for k being the hash-key of pn
                                                        collect (cons k (car (gethash k pn))))
                              for ch = (choose-letter)
                              while ch
                              collect ch
                              do (setf pn (cdr (gethash ch pn)))))))))
