
(defun parse-number-string (s)
  (read-from-string (format nil "(~A)" (string-trim '(#\Space #\Tab #\Newline #\Return) s))))

(defun calculate-points (winning-nums your-nums)
  (let ((winning-ht (make-hash-table :test 'eql))
        (points 0))
    (dolist (num winning-nums)
      (setf (gethash num winning-ht) t))
    (dolist (num your-nums)
      (when (gethash num winning-ht)
        (if (zerop points)
            (setf points 1)
            (setf points (* points 2)))))
    points))

(defun main ()
  (let ((total-points 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (let* ((pipe-pos (position #\| line))
                      (colon-pos (position #\: line))
                      (winning-str (subseq line (1+ colon-pos) pipe-pos))
                      (your-str (subseq line (1+ pipe-pos))))
                 (incf total-points
                       (calculate-points
                        (parse-number-string winning-str)
                        (parse-number-string your-str))))))
    (format t "~A~%" total-points)))

(main)
