
(defun next-secret (s)
  (let* ((x (* s 64))
         (s (logxor s x))
         (s (logand s #xFFFFFF))
         (x (truncate s 32))
         (s (logxor s x))
         (s (logand s #xFFFFFF))
         (x (* s 2048))
         (s (logxor s x))
         (s (logand s #xFFFFFF)))
    s))

(defun main ()
  (let ((total 0))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            do (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (when (not (string= "" trimmed-line))
                   (let ((b (parse-integer trimmed-line)))
                     (loop repeat 2000
                           do (setq b (next-secret b)))
                     (incf total b))))))
    (format t "~a~%" total)))

(main)
