
(defun josephus (n)
  (loop with i = 1
        while (<= (* i 3) n)
        do (setf i (* i 3))
        finally (return (+ (- n i) (max (- n (* 2 i)) 0)))))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((line (read-line f))
           (num-elves (parse-integer line)))
      (format t "~a~%" (josephus num-elves)))))

(main)
