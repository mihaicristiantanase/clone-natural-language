(in-package #:cloned-natural-language)

;; tests

(defun equal-approx (x y)
  "Documentation for equal-approx with parameters x y"
  (<= (- x 4) y (+ x 4)))

(defun test-choose-letter1 (&aux (*percentages* '((#\a . 30) (#\b . 60) (#\c . 10))))
  "Documentation for test-choose-letter1 with parameters "
  (let* ((trials 10000)
         (results (loop for tries below trials collect (choose-letter))))
    (let* ((chars (remove-duplicates results))
           (percentages (loop for ch in chars
                              collect (cons ch (round (* (count ch results) 100) trials)))))
      (unless (equal-approx 100 (reduce '+ percentages :key #'cdr))
        (error "Wrong characters generated: ~a" percentages))
      (loop for (ch . expected-perc) in *percentages*
            for got-perc = (or (cdr (assoc ch percentages)) 0) do
              (unless (equal-approx expected-perc got-perc)
                (error "Wrong percentages for '~a'. Expected ~a got ~a"
                       ch expected-perc got-perc)))))
  t)
