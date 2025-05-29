
(defun parse-input (filename)
  (let (left-list right-list)
    (with-open-file (file filename :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            when (string-trim '(#\Space #\Tab #\Newline #\Return) line)
              do (with-input-from-string (s line)
                   (let ((left (read s))
                         (right (read s)))
                     (push left left-list)
                     (push right right-list)))))
    (values (nreverse left-list) (nreverse right-list))))

(defun calculate-similarity-score (left-list right-list)
  (let ((right-frequency (make-hash-table :test 'eql)))
    (loop for num in right-list
          do (incf (gethash num right-frequency 0)))
    (loop for num in left-list
          sum (* num (gethash num right-frequency 0)))))

(defun main ()
  (multiple-value-bind (left-list right-list) (parse-input "input.txt")
    (let ((similarity-score (calculate-similarity-score left-list right-list)))
      (format t "~a~%" similarity-score))))

(main)
