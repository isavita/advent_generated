
(defun main ()
  (let* ((data (with-open-file (f "input.txt")
                 (read-line f)))
         (target (floor (parse-integer data) 11))
         (houses (make-array (1+ target) :initial-element 0)))
    (loop for elf from 1 to target do
      (let ((upper-bound (min (* elf 50) target)))
        (loop for house from elf by elf upto upper-bound do
          (incf (aref houses house) elf))))
    (loop for house-number from 1 upto target do
      (when (>= (aref houses house-number) target)
        (print house-number)
        (return)))))

(main)
