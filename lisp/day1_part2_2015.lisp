
(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (loop with floor = 0
          for position from 1
          for char = (read-char stream nil :eof)
          until (eq char :eof)
          do
             (cond
               ((char= char #\() (incf floor))
               ((char= char #\)) (decf floor)))
             (when (= floor -1)
               (format t "~a~%" position)
               (return)))))

(main)
