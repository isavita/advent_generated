
(defun calculate-memory-length (s)
  (let ((len (length s))
        (memory-len 0)
        (i 1))
    (loop while (< i (- len 1))
          do (let ((c (char s i)))
               (cond
                 ((char= c #\\)
                  (incf i)
                  (let ((next-c (char s i)))
                    (cond
                      ((or (char= next-c #\\) (char= next-c #\"))
                       (incf memory-len)
                       (incf i))
                      ((char= next-c #\x)
                       (incf memory-len)
                       (incf i 3))
                      (t
                       (error "Unhandled escape sequence in memory length: ~a" (subseq s i (min (+ i 2) len)))))))
                 (t
                  (incf memory-len)
                  (incf i)))))
    memory-len))

(defun calculate-encoded-length (s)
  (let ((res-len (+ (length s) 2)))
    (loop for c across s
          do (cond
               ((char= c #\\) (incf res-len))
               ((char= c #\") (incf res-len))))
    res-len))

(defun main ()
  (let ((total-code 0)
        (total-memory 0)
        (total-encoded 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil)
            while line
            do (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (when (> (length trimmed-line) 0)
                   (incf total-code (length trimmed-line))
                   (incf total-memory (calculate-memory-length trimmed-line))
                   (incf total-encoded (calculate-encoded-length trimmed-line))))))
    (format t "~a~%" (- total-code total-memory))
    (format t "~a~%" (- total-encoded total-code))))

#+sbcl (main)
#+clozure (main)
#+ecl (main)
#+abcl (main)
#+lispworks (main)
#+allegro (main)
