
(defun read-file-content (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (read-line stream nil :eof)))

(defun solve-captcha (input-string)
  (let* ((len (length input-string))
         (sum 0))
    (loop for i from 0 below len
          do (let ((current-digit (- (char-int (char input-string i)) (char-int #\0)))
                   (next-digit (- (char-int (char input-string (mod (+ i 1) len))) (char-int #\0))))
               (when (= current-digit next-digit)
                 (incf sum current-digit))))
    sum))

(defun main ()
  (handler-case
      (let ((file-content (read-file-content "input.txt")))
        (cond ((eq file-content :eof)
               (error "Input file 'input.txt' is empty or missing content."))
              (t
               (format t "~a~%" (solve-captcha file-content)))))
    (error (e)
      (format *error-output* "~a~%" e))))

(main)
