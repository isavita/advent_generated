
(defun read-file-contents (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let* ((file-length (file-length stream))
           (buffer (make-string file-length)))
      (read-sequence buffer stream)
      buffer)))

(defun decompress (input)
  (let ((len (length input))
        (total-length 0)
        (i 0))
    (loop while (< i len) do
      (let ((char (char input i)))
        (cond
          ((char= char #\()
           (let* ((j (position #\) input :start (+ i 1)))
                  (marker-str (subseq input (+ i 1) j))
                  (x-pos (position #\x marker-str))
                  (a (parse-integer marker-str :start 0 :end x-pos))
                  (b (parse-integer marker-str :start (+ x-pos 1))))
             (incf total-length (* a b))
             (setf i (+ j a 1))))
          (t
           (incf total-length)
           (incf i)))))
    total-length))

(defun main ()
  (handler-case
      (let* ((input-str (read-file-contents "input.txt"))
             (decompressed-len (decompress input-str)))
        (format t "~a~%" decompressed-len))
    (error (c)
      (format *error-output* "Error reading the file: ~a~%" c))))

(main)
