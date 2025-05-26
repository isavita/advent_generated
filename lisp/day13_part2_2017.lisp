
(defun read-firewall-data (filename)
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil)
          while line
          collect (let* ((colon-pos (position #\: line))
                         (depth (parse-integer (subseq line 0 colon-pos)))
                         (range (parse-integer (subseq line (+ colon-pos 2)))))
                    (cons depth (- (* 2 range) 2))))))

(defun caught (delay firewall-layers)
  (loop for layer in firewall-layers
        do (let* ((depth (car layer))
                  (period (cdr layer)))
             (when (zerop (mod (+ delay depth) period))
               (return t)))
        finally (return nil)))

(defun main ()
  (let* ((firewall-layers (read-firewall-data "input.txt"))
         (delay 0))
    (loop while (caught delay firewall-layers)
          do (incf delay))
    (format t "~a~%" delay)))

(main)
