
(defun main ()
  (let ((checksum 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil :eof)
            until (eq line :eof)
            do (with-input-from-string (s line)
                 (let* ((first-num (read s nil nil))
                        (min-val first-num)
                        (max-val first-num))
                   (when first-num
                     (loop for num = (read s nil nil)
                           while num
                           do (setf min-val (min min-val num)
                                    max-val (max max-val num)))
                     (incf checksum (- max-val min-val)))))))
    (format t "~a~%" checksum)))

(main)
