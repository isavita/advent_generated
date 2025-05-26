
(defun scanner-position-at-time (rng time)
  (let ((cycle-length (* 2 (- rng 1))))
    (if (= cycle-length 0)
        0
        (let ((time-in-cycle (mod time cycle-length)))
          (if (< time-in-cycle rng)
              time-in-cycle
              (- cycle-length time-in-cycle))))))

(defun solve ()
  (let ((firewall (make-hash-table))
        (max-depth 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil) while line do
        (let* ((colon-pos (position #\: line))
               (depth (parse-integer line :end colon-pos))
               (rng (parse-integer line :start (+ colon-pos 2))))
          (setf (gethash depth firewall) rng)
          (setf max-depth (max max-depth depth)))))

    (let ((severity 0))
      (loop for depth from 0 to max-depth do
        (let ((rng (gethash depth firewall)))
          (when rng
            (when (= (scanner-position-at-time rng depth) 0)
              (setf severity (+ severity (* depth rng)))))))
      (format t "~a~%" severity))))

(defun main ()
  (solve))

(main)
