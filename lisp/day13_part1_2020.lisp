
(defun split-string (str delim)
  (loop with pos = 0
        with len = (length str)
        for next-delim = (position delim str :start pos)
        collect (subseq str pos (or next-delim len))
        do (if next-delim
               (setf pos (1+ next-delim))
               (loop-finish))))

(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun solve ()
  (let* ((input-lines (read-file-lines "input.txt"))
         (earliest-timestamp (parse-integer (first input-lines)))
         (bus-ids (mapcar #'parse-integer
                          (remove-if (lambda (id-str) (string= id-str "x"))
                                     (split-string (second input-lines) #\,))))
         (earliest-bus-id 0)
         (min-wait-time most-positive-fixnum))
    (loop for bus-id in bus-ids do
          (let ((wait-time (- bus-id (mod earliest-timestamp bus-id))))
            (when (< wait-time min-wait-time)
              (setf min-wait-time wait-time
                    earliest-bus-id bus-id))))
    (* earliest-bus-id min-wait-time)))

(defun main ()
  (print (solve)))

(main)
