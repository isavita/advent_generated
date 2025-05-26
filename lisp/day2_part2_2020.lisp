
(defun split-string (string delimiter)
  (loop with result = '()
        with start = 0
        for pos = (search delimiter string :start2 start)
        if pos
          do (push (subseq string start pos) result)
             (setq start (+ pos (length delimiter)))
        else
          do (push (subseq string start) result)
             (return (nreverse result))))

(defun main ()
  (let ((valid-count 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((line-parts (split-string line ": "))
                      (policy-str (first line-parts))
                      (password (second line-parts))
                      (policy-parts (split-string policy-str " "))
                      (positions-str (first policy-parts))
                      (letter-char (char (second policy-parts) 0))
                      (pos-parts (split-string positions-str "-"))
                      (pos1 (parse-integer (first pos-parts)))
                      (pos2 (parse-integer (second pos-parts))))
                 (let ((char-at-pos1 (char password (1- pos1)))
                       (char-at-pos2 (char password (1- pos2))))
                   (when (not (eq (char= char-at-pos1 letter-char)
                                  (char= char-at-pos2 letter-char)))
                     (incf valid-count))))))
    (format t "~a~%" valid-count)))

(main)
