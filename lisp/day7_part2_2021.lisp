
(defun read-input (filename)
  (with-open-file (f filename)
    (let ((line (read-line f))
          (numbers '()))
      (loop with start = 0
            for end = (position #\, line :start start)
            do (push (parse-integer (subseq line start (or end (length line)))) numbers)
            while end
            do (setf start (1+ end)))
      (nreverse numbers))))

(defun calculate-fuel-part-one (positions target)
  (loop for pos in positions
        sum (abs (- pos target))))

(defun calculate-fuel-part-two (positions target)
  (loop for pos in positions
        for distance = (abs (- pos target))
        sum (floor (* distance (+ distance 1)) 2)))

(defun find-optimal-position-part-one (positions min-pos max-pos)
  (loop for target from min-pos to max-pos
        minimize (calculate-fuel-part-one positions target)))

(defun find-optimal-position-part-two (positions min-pos max-pos)
  (loop for target from min-pos to max-pos
        minimize (calculate-fuel-part-two positions target)))

(defun main ()
  (let* ((positions (read-input "input.txt"))
         (min-val (apply #'min positions))
         (max-val (apply #'max positions)))
    (format t "Minimum fuel for part one: ~a~%" (find-optimal-position-part-one positions min-val max-val))
    (format t "Minimum fuel for part two: ~a~%" (find-optimal-position-part-two positions min-val max-val))))

(main)
