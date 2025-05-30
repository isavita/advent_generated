
(defun split-by-comma (s)
  (loop for i = 0 then (1+ j)
        for j = (position #\, s :start i)
        collect (subseq s i j)
        while j))

(defun get-points (wire-moves)
  (let ((points (make-hash-table :test 'equal))
        (x 0)
        (y 0))
    (dolist (move wire-moves)
      (let* ((direction (char move 0))
             (distance (parse-integer (subseq move 1))))
        (dotimes (i distance)
          (cond
            ((char= direction #\R) (incf x))
            ((char= direction #\L) (decf x))
            ((char= direction #\U) (incf y))
            ((char= direction #\D) (decf y)))
          (setf (gethash (list x y) points) t))))
    points))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((w1-str (read-line f))
           (w2-str (read-line f))
           (w1-moves (split-by-comma w1-str))
           (w2-moves (split-by-comma w2-str))
           (w1-points (get-points w1-moves))
           (w2-points (get-points w2-moves))
           (min-distance most-positive-fixnum))
      (let ((smaller-points (if (< (hash-table-count w1-points) (hash-table-count w2-points))
                                w1-points
                                w2-points))
            (larger-points (if (< (hash-table-count w1-points) (hash-table-count w2-points))
                               w2-points
                               w1-points)))
        (maphash (lambda (point-key value)
                   (declare (ignore value))
                   (when (gethash point-key larger-points)
                     (let ((x (first point-key))
                           (y (second point-key)))
                       (when (or (/= x 0) (/= y 0))
                         (let ((dist (+ (abs x) (abs y))))
                           (when (< dist min-distance)
                             (setf min-distance dist)))))))
                 smaller-points))
      (format t "~a~%" min-distance))))

(main)
