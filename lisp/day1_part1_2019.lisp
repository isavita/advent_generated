
(defun main ()
  (with-open-file (s "input.txt")
    (loop for l = (read-line s nil nil)
          while l
          sum (- (floor (parse-integer l) 3) 2))))

(print (main))
