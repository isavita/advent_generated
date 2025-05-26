
(defun seat-id (boarding-pass)
  (let* ((row-part (subseq boarding-pass 0 7))
         (col-part (subseq boarding-pass 7 10))
         (row-binary-str (substitute #\1 #\B (substitute #\0 #\F row-part)))
         (col-binary-str (substitute #\1 #\R (substitute #\0 #\L col-part))))
    (+ (* (parse-integer row-binary-str :radix 2) 8)
       (parse-integer col-binary-str :radix 2))))

(defun main ()
  (let ((max-id 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (setf max-id (max max-id (seat-id line)))))
    (format t "~a~%" max-id)))

(main)
