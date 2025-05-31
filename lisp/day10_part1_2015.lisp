
(defun read-input (filename)
  (with-open-file (s filename)
    (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line s nil ""))))

(defun next-sequence (seq)
  (let ((res (make-string-output-stream)))
    (loop with len = (length seq)
          with i = 0
          while (< i len)
          do (let* ((c (char seq i))
                    (count 1))
               (loop for j from (1+ i) below len
                     while (char= (char seq j) c)
                     do (incf count))
               (format res "~a~a" count c)
               (incf i count)))
    (get-output-stream-string res)))

(defun look-and-say (seq iterations)
  (loop repeat iterations
        do (setf seq (next-sequence seq)))
  seq)

(defun main ()
  (let* ((initial-seq (read-input "input.txt"))
         (final-seq (look-and-say initial-seq 40)))
    (print (length final-seq))))

(main)
