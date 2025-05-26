
(defun calculate-fuel (mass)
  (loop with total-fuel = 0
        for current-mass = mass then fuel-needed
        for fuel-needed = (- (floor current-mass 3) 2)
        when (<= fuel-needed 0)
          do (loop-finish)
        do (incf total-fuel fuel-needed)
        finally (return total-fuel)))

(defun main ()
  (with-open-file (in "input.txt" :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          sum (calculate-fuel (parse-integer line)))))

(print (main))
