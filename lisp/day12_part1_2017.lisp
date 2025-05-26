
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun find-group (program pipes visited)
  (unless (gethash program visited)
    (setf (gethash program visited) t)
    (loop for p in (gethash program pipes)
          do (find-group p pipes visited))))

(defun main ()
  (let ((pipes (make-hash-table))
        (visited (make-hash-table)))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (parts (split-string trimmed-line #\Space))
                      (program-id (parse-integer (first parts)))
                      (connected-to (loop for p in (cddr parts)
                                          collect (parse-integer (string-trim '(#\,) p)))))
                 (setf (gethash program-id pipes) connected-to))))
    (find-group 0 pipes visited)
    (print (hash-table-count visited))))

(main)
