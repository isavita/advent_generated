
(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (let ((frequency 0))
      (loop for line = (read-line stream nil)
            while line
            do (incf frequency (parse-integer line)))
      (format t "~a~%" frequency))))

(main)
