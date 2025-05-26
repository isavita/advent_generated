
(defconstant +max-total-distance+ 10000)
(defconstant +input-filename+ "input.txt")

(defun parse-point (s)
  (let* ((comma-pos (position #\, s)))
    (list (parse-integer (string-trim " " (subseq s 0 comma-pos)))
          (parse-integer (string-trim " " (subseq s (1+ comma-pos)))))))

(defun read-coordinates (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-point line))))

(defun manhattan-distance (p1 p2)
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))

(defun closest-coordinate-index (point coordinates)
  (loop with min-dist = most-positive-fixnum
        with closest-idx = nil
        for coord in coordinates
        for i from 0
        for dist = (manhattan-distance point coord)
        when (< dist min-dist)
          do (setf min-dist dist
                   closest-idx i)
        else when (= dist min-dist)
          do (setf closest-idx nil)
        finally (return closest-idx)))

(defun main ()
  (let* ((coordinates (read-coordinates +input-filename+))
         (num-coordinates (length coordinates))
         (max-x (loop for c in coordinates maximize (first c)))
         (max-y (loop for c in coordinates maximize (second c)))
         (areas (make-array num-coordinates :initial-element 0))
         (infinite-areas (make-hash-table :test 'eql))
         (region-size 0))

    (loop for x from 0 to max-x
          do (loop for y from 0 to max-y
                   do (let* ((current-point (list x y))
                             (total-dist (loop for coord in coordinates
                                               sum (manhattan-distance current-point coord)))
                             (closest-idx (closest-coordinate-index current-point coordinates)))

                        (when (< total-dist +max-total-distance+)
                          (incf region-size))

                        (when closest-idx
                          (incf (aref areas closest-idx))
                          (when (or (= x 0) (= x max-x) (= y 0) (= y max-y))
                            (setf (gethash closest-idx infinite-areas) t))))))

    (let ((largest-area (loop for i from 0 below num-coordinates
                              for area = (aref areas i)
                              when (not (gethash i infinite-areas))
                                maximize area)))
      (format t "~a~%" (or largest-area 0))
      (format t "~a~%" region-size))))

(main)
