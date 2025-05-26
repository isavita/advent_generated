
(defun josephus (n)
  (let* ((k (- (integer-length n) 1))
         (p (ash 1 k)))
    (logior (ash (- n p) 1) 1)))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let ((num-elves (read f)))
      (format t "~a~%" (josephus num-elves)))))

(main)
