
(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (loop with current-elf-calories = 0
          with max-calories = 0
          for line = (read-line stream nil nil)
          while line
          do (if (string= line "")
                 (progn
                   (setf max-calories (max max-calories current-elf-calories))
                   (setf current-elf-calories 0))
                 (incf current-elf-calories (parse-integer line)))
          finally (progn
                    (setf max-calories (max max-calories current-elf-calories))
                    (format t "~a~%" max-calories)))))

(main)
