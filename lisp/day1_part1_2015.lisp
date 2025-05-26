(defun calculate-floor (input)
  "Calculate the final floor based on the input string"
  (loop for char across input
        with floor = 0
        do (cond
             ((char= char #\() (incf floor))
             ((char= char #\)) (decf floor)))
        finally (return floor)))

(defun main ()
  "Main function to read input and solve the problem"
  (with-open-file (stream "input.txt" :direction :input)
    (let ((input (read-line stream nil)))
      (when input
        (format t "Final floor: ~d~%" (calculate-floor input))))))

(main)
