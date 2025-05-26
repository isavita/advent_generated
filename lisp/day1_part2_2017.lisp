
(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((data (read-line f nil ""))
           (len (length data))
           (half-len (floor len 2))
           (total 0))
      (loop for i from 0 below len do
        (when (char= (char data i)
                     (char data (mod (+ i half-len) len)))
          (incf total (- (char-code (char data i)) (char-code #\0)))))
      (princ total))))

(main)
