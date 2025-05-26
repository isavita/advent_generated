
(defun main ()
  (let ((numbers (with-open-file (stream "input.txt" :direction :input)
                   (loop for line = (read-line stream nil nil)
                         while line
                         collect (parse-integer line)))))
    (print (loop for (a b) on numbers
                 when (and b (> b a))
                 count t))))

(main)
