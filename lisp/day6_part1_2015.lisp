
(defvar *lights* (make-array '(1000 1000) :initial-element 0))

(defun turn-on (x1 y1 x2 y2)
  (loop for i from x1 to x2 do
    (loop for j from y1 to y2 do
      (setf (aref *lights* i j) 1))))

(defun turn-off (x1 y1 x2 y2)
  (loop for i from x1 to x2 do
    (loop for j from y1 to y2 do
      (setf (aref *lights* i j) 0))))

(defun toggle (x1 y1 x2 y2)
  (loop for i from x1 to x2 do
    (loop for j from y1 to y2 do
      (setf (aref *lights* i j) (- 1 (aref *lights* i j))))))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun parse-coords (s)
  (let ((parts (split-string s #\,)))
    (list (parse-integer (first parts))
          (parse-integer (second parts)))))

(defun main ()
  (with-open-file (file "input.txt" :direction :input)
    (loop for line = (read-line file nil nil)
          while line do
      (let* ((parts (split-string line #\Space))
             (cmd (first parts)))
        (cond
          ((string= cmd "turn")
           (let* ((on-off (second parts))
                  (c1 (parse-coords (third parts)))
                  (c2 (parse-coords (fifth parts)))
                  (x1 (first c1)) (y1 (second c1))
                  (x2 (first c2)) (y2 (second c2)))
             (if (string= on-off "on")
                 (turn-on x1 y1 x2 y2)
                 (turn-off x1 y1 x2 y2))))
          ((string= cmd "toggle")
           (let* ((c1 (parse-coords (second parts)))
                  (c2 (parse-coords (fourth parts)))
                  (x1 (first c1)) (y1 (second c1))
                  (x2 (first c2)) (y2 (second c2)))
             (toggle x1 y1 x2 y2)))))))

  (let ((total-lights-on 0))
    (loop for i from 0 below 1000 do
      (loop for j from 0 below 1000 do
        (incf total-lights-on (aref *lights* i j))))
    (format t "~a~%" total-lights-on)))

(main)
