
(defun main ()
  (let ((current-elf-total 0)
        (top-three-calories (list 0 0 0)))

    (labels ((update-top-three (new-sum)
               (cond ((>= new-sum (first top-three-calories))
                      (setf (third top-three-calories) (second top-three-calories)
                            (second top-three-calories) (first top-three-calories)
                            (first top-three-calories) new-sum))
                     ((>= new-sum (second top-three-calories))
                      (setf (third top-three-calories) (second top-three-calories)
                            (second top-three-calories) new-sum))
                     ((>= new-sum (third top-three-calories))
                      (setf (third top-three-calories) new-sum)))))

      (with-open-file (stream "input.txt" :direction :input)
        (loop for line = (read-line stream nil :eof)
              until (eq line :eof)
              do (if (string= line "")
                     (progn
                       (update-top-three current-elf-total)
                       (setf current-elf-total 0))
                     (incf current-elf-total (parse-integer line))))
        (update-top-three current-elf-total)))

    (print (apply #'+ top-three-calories))))

(main)
