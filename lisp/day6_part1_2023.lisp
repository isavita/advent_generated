
(defun my-split (s delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun parse-numbers-from-line (line)
  (loop for word in (my-split line #\Space)
        for num = (ignore-errors (parse-integer word))
        when (and num (integerp num))
        collect num))

(defun number-of-ways-to-win (time record)
  (let* ((f-time (coerce time 'double-float))
         (f-record (coerce record 'double-float))
         (delta (- (* f-time f-time) (* 4.0d0 f-record))))
    (if (<= delta 0.0d0)
        0
        (let* ((sqrt-delta (sqrt delta))
               (h1 (/ (- f-time sqrt-delta) 2.0d0))
               (h2 (/ (+ f-time sqrt-delta) 2.0d0))
               (first-win-h (ceiling (+ h1 1.0d-9)))
               (last-win-h (floor (- h2 1.0d-9))))
          (max 0 (+ (- last-win-h first-win-h) 1))))))

(defun calculate-ways (times distances)
  (loop for time in times
        for distance in distances
        with total-ways = 1
        do (setf total-ways (* total-ways (number-of-ways-to-win time distance)))
        finally (return total-ways)))

(defun main ()
  (let ((times (list))
        (distances (list))
        (file-path "input.txt"))
    (with-open-file (stream file-path :direction :input :if-does-not-exist :error)
      (let ((line1 (read-line stream))
            (line2 (read-line stream)))
        (setf times (parse-numbers-from-line line1))
        (setf distances (parse-numbers-from-line line2))))
    (let ((total-ways (calculate-ways times distances)))
      (format t "~a~%" total-ways))))

(main)
