
(defun validate-password (policy-str password-str)
  (let* ((p1 (position #\- policy-str))
         (p2 (position #\Space policy-str :start (1+ p1)))
         (min-val (parse-integer policy-str :start 0 :end p1))
         (max-val (parse-integer policy-str :start (1+ p1) :end p2))
         (char-to-find (char policy-str (1+ p2)))
         (count 0))
    (loop for c across password-str
          when (char= c char-to-find)
            do (incf count))
    (and (>= count min-val) (<= count max-val))))

(defun main ()
  (let ((valid-count 0))
    (with-open-file (stream "input.txt" :direction :input
                                         :if-does-not-exist :error)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((colon-pos (position #\: line))
                      (policy-part (subseq line 0 colon-pos))
                      (password-part (subseq line (+ colon-pos 2))))
                 (when (validate-password policy-part password-part)
                   (incf valid-count)))))
    (format t "~a~%" valid-count)))

(main)
