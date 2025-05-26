
(defun main ()
  (let ((h 0)
        (d 0)
        (a 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do
              (let* ((space-pos (position #\Space line))
                     (command (subseq line 0 space-pos))
                     (value (parse-integer (subseq line (1+ space-pos)))))
                (cond ((string= command "forward")
                       (incf h value)
                       (incf d (* a value)))
                      ((string= command "down")
                       (incf a value))
                      ((string= command "up")
                       (decf a value))))))
    (format t "~a~%" (* h d))))

(main)
