
(defun split-string (delimiter string)
  (loop with result = '()
        with start = 0
        for pos = (search delimiter string :start2 start)
        do (push (subseq string start (or pos (length string))) result)
        (when (null pos) (return (nreverse result)))
        (setf start (+ pos (length delimiter)))))

(defun read-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun parse-point-string (s)
  (let ((coords (split-string "," s)))
    (list (parse-integer (first coords))
          (parse-integer (second coords)))))

(defun parse-line (line-str)
  (let* ((parts (split-string " -> " line-str))
         (start-str (first parts))
         (end-str (second parts)))
    (list (parse-point-string start-str)
          (parse-point-string end-str))))

(defun get-points (start end)
  (let* ((x1 (first start))
         (y1 (second start))
         (x2 (first end))
         (y2 (second end))
         (dx (signum (- x2 x1)))
         (dy (signum (- y2 y1)))
         (points '())
         (current-x x1)
         (current-y y1))
    (loop
      (push (list current-x current-y) points)
      (when (and (= current-x x2) (= current-y y2))
        (return))
      (unless (= current-x x2) (incf current-x dx))
      (unless (= current-y y2) (incf current-y dy)))
    (nreverse points)))

(defun count-overlaps (lines)
  (let ((grid (make-hash-table :test 'equal)))
    (loop for line-str in lines
          do (let* ((parsed-line (parse-line line-str))
                    (start (first parsed-line))
                    (end (second parsed-line))
                    (points (get-points start end)))
               (loop for point in points
                     do (incf (gethash point grid 0)))))
    (loop for count being the hash-value of grid
          count (> count 1))))

(defun main ()
  (let* ((lines (read-lines "input.txt"))
         (overlap-count (count-overlaps lines)))
    (format t "~a~%" overlap-count)))

(main)
