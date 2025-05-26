
(defun main ()
  (let ((sum 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (unless (string= (string-trim '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed) line) "")
                 (let ((first-digit nil)
                       (last-digit nil))
                   (loop for char across line
                         do (when (digit-char-p char)
                              (when (null first-digit)
                                (setf first-digit (digit-char-p char)))
                              (setf last-digit (digit-char-p char))))
                   (when (and first-digit last-digit)
                     (incf sum (+ (* first-digit 10) last-digit)))))))
    (princ sum)))

(main)
