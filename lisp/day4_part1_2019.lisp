
(defun check-password (num)
  (let* ((s (princ-to-string num))
         (len (length s))
         (has-adjacent-duplicate nil)
         (prev-digit -1))
    (when (zerop len) (return-from check-password nil))
    (setf prev-digit (digit-char-p (char s 0)))
    (loop for i from 1 below len
          for current-digit = (digit-char-p (char s i))
          do
             (when (< current-digit prev-digit) (return-from check-password nil))
             (when (= current-digit prev-digit) (setf has-adjacent-duplicate t))
             (setf prev-digit current-digit))
    has-adjacent-duplicate))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((line (read-line f))
           (hyphen-pos (position #\- line))
           (start (parse-integer line :end hyphen-pos))
           (end (parse-integer line :start (1+ hyphen-pos))))
      (let ((count 0))
        (loop for num from start to end
              when (check-password num)
                do (incf count))
        (princ count)))))

(main)
