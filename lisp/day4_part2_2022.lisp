
(defun split-string (s char)
  (loop for i = 0 then (1+ j)
        for j = (position char s :start i)
        collect (subseq s i j)
        while j))

(defun parse-range (s)
  (let* ((parts (split-string s #\-))
         (start (parse-integer (first parts)))
         (end (parse-integer (second parts))))
    (list start end)))

(defun main ()
  (let ((count 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (let* ((pair (split-string line #\,))
                      (left (parse-range (first pair)))
                      (right (parse-range (second pair))))
                 (when (and (<= (first left) (second right))
                            (>= (second left) (first right)))
                   (incf count)))))
    (format t "~a~%" count)))

(main)
