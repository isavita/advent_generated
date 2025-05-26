
(defconstant +digit-words+
  '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defun find-first-and-last-digit (line)
  (let ((first-digit nil)
        (last-digit nil)
        (len (length line)))
    (loop for i from 0 below len
          do
             (let* ((char (char line i))
                    (digit-value (digit-char-p char)))
               (cond
                 (digit-value
                  (unless first-digit
                    (setf first-digit digit-value))
                  (setf last-digit digit-value))
                 (t
                  (block word-scan-loop
                    (loop for j from 0 to 9
                          for word = (nth j +digit-words+)
                          when (and (<= (+ i (length word)) len)
                                    (string= word (subseq line i (+ i (length word)))))
                            do
                               (unless first-digit
                                 (setf first-digit j))
                               (setf last-digit j)
                               (return-from word-scan-loop)))))))
    (values first-digit last-digit)))

(defun main ()
  (let ((total-sum 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil)
            while line
            do
               (multiple-value-bind (first-digit last-digit)
                   (find-first-and-last-digit line)
                 (incf total-sum (+ (* 10 first-digit) last-digit)))))
    (format t "~a~%" total-sum)))

(main)
