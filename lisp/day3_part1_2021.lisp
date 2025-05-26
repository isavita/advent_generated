
(defun read-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun calculate-rates (numbers)
  (let* ((num-len (length (first numbers)))
         (gamma-rate-str (make-string num-len))
         (epsilon-rate-str (make-string num-len)))
    (loop for i from 0 below num-len
          do (let ((count-1 0)
                   (count-0 0))
               (loop for num-str in numbers
                     do (if (char= (char num-str i) #\1)
                            (incf count-1)
                            (incf count-0)))
               (setf (char gamma-rate-str i) (if (> count-1 count-0) #\1 #\0))
               (setf (char epsilon-rate-str i) (if (< count-1 count-0) #\1 #\0))))
    (values (parse-integer gamma-rate-str :radix 2)
            (parse-integer epsilon-rate-str :radix 2))))

(defun main ()
  (let* ((data (read-lines "input.txt"))
         (gamma-rate (nth-value 0 (calculate-rates data)))
         (epsilon-rate (nth-value 1 (calculate-rates data))))
    (format t "~a~%" (* gamma-rate epsilon-rate))))

(main)
