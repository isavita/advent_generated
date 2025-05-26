
(defun react-polymer (polymer)
  (loop with stack = nil
        for unit across polymer
        do (if (and stack
                    (let ((top (car stack)))
                      (and (char-equal unit top)
                           (not (char= unit top)))))
               (setf stack (cdr stack))
               (setf stack (cons unit stack)))
        finally (return (length stack))))

(defun read-file-into-string (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let* ((file-length (file-length stream))
           (data (make-string file-length)))
      (read-sequence data stream)
      (string-trim '(#\Space #\Tab #\Newline #\Return) data))))

(defun main ()
  (let* ((polymer (read-file-into-string "input.txt"))
         (result (react-polymer polymer)))
    (format t "~a~%" result)))

(main)
