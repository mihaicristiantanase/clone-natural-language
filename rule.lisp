;; Rule-based generator

(in-package :cloned-natural-language)

(defun word-complete-p (word)
  "Documentation for word-complete-p with parameters word"
  (eq (car (last word)) 'end))

(defparameter *rules*
  `((min-length . ,(lambda (word)
                     (or (not (word-complete-p word)) (> (length word) 2))))
    (max-length . ,(lambda (word &aux (max-len 8))
                     (or (<= (length word) max-len)
                         (and (= (length word) (1+ max-len))
                              (word-complete-p word)))))
    ))

(defun all-rules-apply (word)
  "Documentation for all-rules-apply with parameters word"
  (dolist (rule *rules*)
    (let ((name (car rule))
          (pred (cdr rule)))
      (unless (funcall pred word)
        (return-from all-rules-apply (values nil name)))))
  t)

(defun create-letter-chooser (alphabet)
  "Documentation for create-letter-chooser with parameters alphabet"
  (let ((full-alphabet alphabet)
        word)
    #'(lambda ()
        (let ((alphabet full-alphabet))
          (let ((ch (loop for ch = (rand-elt (append alphabet '(end)))
                          do (setf alphabet (delete ch alphabet))
                          while alphabet
                          when (all-rules-apply (append word (list ch)))
                            return ch)))
            ;; try with end-of-word
            (unless ch
              (when (all-rules-apply (append word '(end)))
                (setf ch 'end)))

            ;; throw errors
            (when (and (null ch) (null alphabet))
              (error "Could not choose a lettter (word:~{~a~}). ~
                      Are the rules conflicting?" word))

            ;; update current word
            (unless (eq ch 'end)
              (setf word (nreverse (append word (list ch))))
              ch))))))

(defun create-word-from-rules ()
  "Documentation for create-word-from-rules"
  (let ((generator (create-letter-chooser (copy-list *alphabet*))))
    (format nil "~{~a~}"
            (loop for ch = (funcall generator)
                  while ch
                  collect ch))))
