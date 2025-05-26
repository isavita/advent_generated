
(defun read-file-as-string (filename)
  (with-open-file (in filename :direction :input :element-type 'character)
    (let* ((file-length (file-length in))
           (string (make-string file-length)))
      (read-sequence string in)
      string)))

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun main ()
  (let* ((data (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (read-file-as-string "input.txt")))
         (steps (split-string data #\,)))
    (format t "~a~%"
            (loop for step in steps
                  sum (reduce (lambda (current-value char)
                                (mod (* (+ current-value (char-code char)) 17) 256))
                              step
                              :initial-value 0)))))

(main)
