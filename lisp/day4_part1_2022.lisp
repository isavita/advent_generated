
(defun parse-line (line)
  (let* ((comma-pos (position #\, line))
         (range1-str (subseq line 0 comma-pos))
         (range2-str (subseq line (1+ comma-pos)))
         (dash-pos1 (position #\- range1-str))
         (dash-pos2 (position #\- range2-str))
         (start1 (parse-integer range1-str :end dash-pos1))
         (end1 (parse-integer range1-str :start (1+ dash-pos1)))
         (start2 (parse-integer range2-str :end dash-pos2))
         (end2 (parse-integer range2-str :start (1+ dash-pos2))))
    (values start1 end1 start2 end2)))

(defun solve ()
  (let ((count 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (multiple-value-bind (s1 e1 s2 e2) (parse-line line)
                 (when (or (and (<= s1 s2) (>= e1 e2))
                           (and (<= s2 s1) (>= e2 e1)))
                   (incf count)))))
    count))

(defun main ()
  (format t "~a~%" (solve)))

(main)
