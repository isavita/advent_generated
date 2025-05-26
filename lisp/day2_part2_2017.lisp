
(defun read-data (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil)
          while line
          collect (with-input-from-string (s line)
                    (loop for num = (read s nil nil)
                          while num
                          collect num)))))

(defun solve-part1 (data)
  (loop for row in data
        sum (- (apply #'max row) (apply #'min row))))

(defun solve-part2 (data)
  (loop with result = 0
        for row-orig in data
        for row = (sort (copy-list row-orig) #'<)
        do (loop for x in row
                 for i from 0
                 do (loop for y in (nthcdr (+ i 1) row)
                          when (and (not (zerop x)) (zerop (mod y x)))
                          do (incf result (floor y x))))
        finally (return result)))

(defun main ()
  (let* ((data (read-data "input.txt"))
         (part1-result (solve-part1 data))
         (part2-result (solve-part2 data)))
    (format t "~a~%" part1-result)
    (format t "~a~%" part2-result)))

(main)
