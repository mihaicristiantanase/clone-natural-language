;;; Choose letters based on n-gram statistics

(in-package #:cloned-natural-language)

;; TODO(mihai): handle end of word with special symbol
(defun load-2-gram (words)
  "Documentation for load-2-gram with parameters words"
  (let ((rv (make-hash-table)))
    (loop for word in words
          for prev-ch = nil do
            (loop for ch across word do
              (when prev-ch
                (let ((freqs (gethash prev-ch rv)))
                  (unless freqs
                    (setf freqs (list (list)))
                    (setf (gethash prev-ch rv) freqs))
                  (let ((fr (assoc ch freqs)))
                    (unless fr
                      (setf fr (cons ch 0))
                      (if (null (car freqs))
                          (setf (car freqs) fr)
                          (setf (cdr (last freqs)) (list fr))))
                    (incf (cdr fr)))))
              (setf prev-ch ch)))
    rv))

(defun convert-to-percentages-in-ngram (n-gram)

  (loop for v being the hash-value of n-gram
        for sum = (reduce '+ v :key 'cdr) do
          (loop for item in v do
            (setf (cdr item) (round (* (cdr item) 100) sum))))
  n-gram)

(defun load-2-gram-from-file (fname)
  "Documentation for load-ngram with parameters fname"
  (with-open-file (f fname)
    (let ((words (loop for line = (read-line f nil nil nil) while line
                       collect line)))
      (convert-to-percentages-in-ngram
       (load-2-gram words)))))

(defparameter *percentages-per-2-gram*
  (load-2-gram-from-file "română.txt"))

(defun create-word-from-2-gram ()
  "Documentation for create-word-from-2-gram with parameters "
  (let ((*percentages*
          (mapcar #'(lambda (k) (cons k (round 100 (hash-table-count *percentages-per-2-gram*))))
                  (loop for k being the hash-key of *percentages-per-2-gram*
                        collect k))))
    (format nil "~{~a~}"
            (loop for ch = (choose-letter)
                  while ch
                  collect ch
                  do (setf *percentages* (gethash ch *percentages-per-2-gram*))))))
