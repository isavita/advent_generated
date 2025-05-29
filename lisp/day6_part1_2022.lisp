
(defun slurp-file (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let* ((size (file-length stream))
           (buffer (make-string size)))
      (read-sequence buffer stream)
      buffer)))

(defun find-start-of-packet-marker (datastream)
  (let ((len (length datastream)))
    (loop for i from 3 below len
          for c1 = (char datastream (- i 3))
          for c2 = (char datastream (- i 2))
          for c3 = (char datastream (- i 1))
          for c4 = (char datastream i)
          when (and (char/= c1 c2)
                    (char/= c1 c3)
                    (char/= c1 c4)
                    (char/= c2 c3)
                    (char/= c2 c4)
                    (char/= c3 c4))
            do (return (+ i 1)))))

(defun main ()
  (let* ((data (string-trim '(#\Newline #\Return) (slurp-file "input.txt")))
         (position (find-start-of-packet-marker data)))
    (print position)))

(main)
