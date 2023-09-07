;;; Choose letters based on n-gram statistics

(in-package #:cloned-natural-language)

(defun append-keeping-last (str c n)
  "Documentation for append-keeping-last with parameters str c n"
  (let* ((rv (concatenate 'string str (list c)))
         (len (length rv)))
    (if (> len n)
        (subseq rv (- len n))
        rv)))

(defun load-n-gram (words &optional (n 2))
  "Documentation for load-n-gram with parameters words and n (the gram order)"
  (decf n)
  (let ((rv (make-hash-table :test 'equal)))
    (flet ((merge-freqs (prev-gram ch)
             (let ((freqs (gethash prev-gram rv)))
               (unless freqs
                 (setf freqs (list (list)))
                 (setf (gethash prev-gram rv) freqs))
               (let ((fr (assoc ch freqs)))
                 (unless fr
                   (setf fr (cons ch 0))
                   (if (null (car freqs))
                       (setf (car freqs) fr)
                       (setf (cdr (last freqs)) (list fr))))
                 (incf (cdr fr))))))
      (loop for word in words
            for prev-gram = "" do
              (loop for ch across word do
                (when (= (length prev-gram) n)
                  (merge-freqs prev-gram ch))
                (setf prev-gram (append-keeping-last prev-gram ch n)))
              (when (= (length prev-gram) n)
                (merge-freqs prev-gram nil))))
    rv))

(defun convert-to-percentages-in-ngram (n-gram)
  (loop for v being the hash-value of n-gram
        for sum = (reduce '+ v :key 'cdr) do
          (loop for item in v do
            (setf (cdr item) (round (* (cdr item) 100) sum))))
  n-gram)

(defun load-n-gram-from-file (fname n)
  "Documentation for load-ngram with parameters fname"
  (with-open-file (f fname)
    (let ((words (loop for line = (read-line f nil nil nil) while line
                       collect line)))
      (convert-to-percentages-in-ngram
       (load-n-gram words n)))))

(defparameter *percentages-per-2-gram*
  (load-n-gram-from-file *word-file* 2))

(defparameter *percentages-per-3-gram*
  (load-n-gram-from-file *word-file* 3))

(defun create-word-from-n-gram ()
  "Documentation for create-word-from-n-gram with parameters "
  (let ((*percentages*
          (mapcar #'(lambda (k) (cons k (round 100 (hash-table-count *percentages-per-2-gram*))))
                  (loop for k being the hash-key of *percentages-per-2-gram*
                        collect (aref k 0)))))
    (format nil "~{~a~}"
            (let ((prev-gram ""))
              (loop for len from 0
                    for ch = (if (< len 2)
                                 ;; keep choosing until a non-nil value is returned
                                 (loop for rv = (choose-letter)
                                       when rv return rv)
                                 ;; choose whatever comes
                                 (choose-letter))
                    while ch
                    collect ch
                    do (setf prev-gram (append-keeping-last prev-gram ch 2))
                       (setf *percentages*
                             (gethash prev-gram
                                      (if (> (length prev-gram) 1)
                                          *percentages-per-3-gram*
                                          *percentages-per-2-gram*))))))))
