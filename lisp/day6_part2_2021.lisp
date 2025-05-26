
(defun main ()
  (let ((lantern-fish-counts (make-array 9 :initial-element 0)))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil :eof)
            until (eq line :eof)
            do (loop for age-str in (split-string line #\,)
                     do (incf (aref lantern-fish-counts (parse-integer age-str))))))
    (loop for day from 1 to 256
          do (let ((new-fish (aref lantern-fish-counts 0)))
               (loop for j from 0 to 7
                     do (setf (aref lantern-fish-counts j) (aref lantern-fish-counts (1+ j))))
               (incf (aref lantern-fish-counts 6) new-fish)
               (setf (aref lantern-fish-counts 8) new-fish)))
    (format t "~a~%" (loop for count across lantern-fish-counts summing count))))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(main)
