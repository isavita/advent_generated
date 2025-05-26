
(defun main ()
  (let ((h 0)
        (d 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (with-input-from-string (s line)
                 (let ((action (read s))
                       (value (read s)))
                   (case action
                     ((forward) (incf h value))
                     ((down) (incf d value))
                     ((up) (decf d value)))))))
    (format t "~a~%" (* h d))))

(main)
