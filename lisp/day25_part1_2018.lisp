
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ pos)
        for pos = (position delimiter string :start start)
        collect (subseq string start (or pos (length string)))
        while pos))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (mapcar #'parse-integer (split-string line #\,)))))

(defun manhattan-distance (point1 point2)
  (loop for coord1 in point1
        for coord2 in point2
        sum (abs (- coord1 coord2))))

(defun find-constellations (points-list)
  (let* ((num-points (length points-list))
         (points (make-array num-points :initial-contents points-list))
         (visited (make-array num-points :element-type 'boolean :initial-element nil))
         (constellations 0))
    (labels ((dfs (index)
               (setf (aref visited index) t)
               (loop for i from 0 below num-points
                     when (and (not (aref visited i))
                               (<= (manhattan-distance (aref points index) (aref points i)) 3))
                       do (dfs i))))
      (loop for i from 0 below num-points
            when (not (aref visited i))
              do (incf constellations)
                 (dfs i)))
    constellations))

(defun main ()
  (let* ((points (read-input "input.txt"))
         (result (find-constellations points)))
    (format t "~a~%" result)))

(main)
