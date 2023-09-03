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

(defparameter *alphabet*
  #(#\a #\b #\c #\d #\e #\f #\g #\h #\i
    #\j #\k #\l #\m #\n #\o #\p #\q #\r
    #\s #\t #\u #\v #\w #\x #\y #\z #\ă
    #\â #\î #\ț #\ș))

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

(defun rand-elt (lst)
  "Documentation for rand-elt with parameters lst"
  (nth (random (length lst)) lst))

(defun create-word-from-randomness ()
  "Documentation for create-word with parameters "
  (let ((len (+ 2 (random 8))))
    (format nil "~{~a~}"
            (loop for c below len
                  collect (aref *alphabet*
                                (if (/= 0 (mod c 2))
                                    (random (length *alphabet*))
                                    (rand-elt '(0 4 8 14 20 26 27 28))))))))

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

;; (dolist (x '(#\a #\e #\i #\o #\u #\ă #\â #\î))
;;            (format t "~a " (search (list x) cloned-natural-language::*alphabet*)))
