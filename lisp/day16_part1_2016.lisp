
(defun dragon-curve (s)
  (let* ((len (length s))
         (b-str (make-string len)))
    (loop for i from 0 below len
          for char = (char s i)
          do (setf (char b-str (- (1- len) i))
                   (case char
                     (#\0 #\1)
                     (#\1 #\0))))
    (format nil "~a0~a" s b-str)))

(defun checksum (data)
  (let ((len (length data)))
    (if (oddp len)
        data
        (let* ((result-len (/ len 2))
               (result-chars (make-string result-len)))
          (loop for i from 0 by 2 below len
                for j from 0
                do (setf (char result-chars j)
                         (if (char= (char data i) (char data (1+ i)))
                             #\1
                             #\0)))
          (if (evenp result-len)
              (checksum result-chars)
              result-chars)))))

(defun main ()
  (let ((initial-state "")
        (target-length 272))
    (with-open-file (file "input.txt" :direction :input)
      (setf initial-state (read-line file nil "")))
    (loop for data = initial-state then (dragon-curve data)
          while (< (length data) target-length)
          finally (progn
                    (setf data (subseq data 0 target-length))
                    (format t "~a~%" (checksum data))))))

(main)
