
(defun count-orbits (orbit-map start depth)
  (let ((orbiters (gethash start orbit-map)))
    (if (null orbiters)
        depth
        (loop for orbiter in orbiters
              sum (count-orbits orbit-map orbiter (+ depth 1)) into total-sub-orbits
              finally (return (+ depth total-sub-orbits))))))

(defun main ()
  (let ((orbit-map (make-hash-table :test 'equal)))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((paren-pos (position #\) line))
                      (center (subseq line 0 paren-pos))
                      (orbiter (subseq line (1+ paren-pos))))
                 (push orbiter (gethash center orbit-map nil)))))
    (let ((total-orbits (count-orbits orbit-map "COM" 0)))
      (format t "~a~%" total-orbits))))

(main)
