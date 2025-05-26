
(defun has-abba (s i)
  (let ((c1 (char s i))
        (c2 (char s (+ i 1)))
        (c3 (char s (+ i 2)))
        (c4 (char s (+ i 3))))
    (and (char/= c1 c2)
         (char= c1 c4)
         (char= c2 c3))))

(defun supports-tls (ip)
  (let ((len (length ip))
        (hypernet nil)
        (outside nil)
        (inside nil))
    (loop for i from 0 below len
          do (let ((c (char ip i)))
               (cond
                 ((char= c #\[)
                  (setf hypernet t))
                 ((char= c #\])
                  (setf hypernet nil))
                 ((<= (+ i 3) (1- len))
                  (when (has-abba ip i)
                    (if hypernet
                        (setf inside t)
                        (setf outside t)))))))
    (and outside (not inside))))

(defun main ()
  (let ((count 0))
    (with-open-file (in "input.txt" :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            do (when (supports-tls line)
                 (incf count))))
    (format t "~a~%" count)))

(main)
