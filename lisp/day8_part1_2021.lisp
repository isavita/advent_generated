
(defun split-string (string delimiter)
  (loop for start = 0 then (+ pos (length delimiter))
        for pos = (search delimiter string :start2 start)
        collect (subseq string start (or pos (length string)))
        while pos))

(defun main ()
  (let ((count 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (parts (split-string trimmed-line " | "))
                      (output (second parts)))
                   (loop for digit-str in (split-string output " ")
                         do (when (member (length digit-str) '(2 4 3 7))
                              (incf count))))))
    (format t "~a~%" count)))

(main)
