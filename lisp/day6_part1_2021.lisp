
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil nil))
           (parts (split-string line #\,)))
      (mapcar #'parse-integer parts))))

(defun simulate-lanternfish (initial-timers days)
  (let ((fish-count (make-array 9 :initial-element 0)))
    (loop for timer in initial-timers
          do (incf (aref fish-count timer)))
    (dotimes (day days)
      (let ((new-fishes (aref fish-count 0)))
        (loop for i from 0 to 7
              do (setf (aref fish-count i) (aref fish-count (1+ i))))
        (incf (aref fish-count 6) new-fishes)
        (setf (aref fish-count 8) new-fishes)))
    (loop for count across fish-count
          summing count)))

(defun main ()
  (let* ((initial-timers (read-input "input.txt"))
         (total-fish (simulate-lanternfish initial-timers 80)))
    (format t "~a~%" total-fish)))

(main)
