
(defun split-string (s delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun get-points-with-steps (path-str)
  (let ((points (make-hash-table :test 'equal))
        (current (list 0 0))
        (steps 0))
    (loop for move-str in (split-string path-str #\,)
          do (let* ((direction (char move-str 0))
                    (distance (parse-integer (subseq move-str 1))))
               (loop repeat distance
                     do (incf steps)
                        (cond ((char= direction #\U) (incf (second current)))
                              ((char= direction #\D) (decf (second current)))
                              ((char= direction #\L) (decf (first current)))
                              ((char= direction #\R) (incf (first current))))
                        (unless (gethash (copy-list current) points)
                          (setf (gethash (copy-list current) points) steps)))))
    points))

(defun main ()
  (let* ((wire-paths (with-open-file (file "input.txt" :direction :input)
                       (loop for line = (read-line file nil nil)
                             while line
                             collect (string-trim '(#\Space #\Newline #\Return) line))))
         (wire1 (get-points-with-steps (first wire-paths)))
         (wire2 (get-points-with-steps (second wire-paths)))
         (min-steps most-positive-fixnum))

    (maphash (lambda (point steps1)
               (multiple-value-bind (steps2 found) (gethash point wire2)
                 (when found
                   (let ((total-steps (+ steps1 steps2)))
                     (when (< total-steps min-steps)
                       (setf min-steps total-steps))))))
             wire1)
    (format t "~a~%" min-steps)))

(main)
