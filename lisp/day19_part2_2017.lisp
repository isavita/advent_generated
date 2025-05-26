
(defun solve ()
  (let* ((grid (coerce (with-open-file (f "input.txt" :direction :input)
                          (loop for line = (read-line f nil nil)
                                while line
                                collect line))
                       'vector))
         (x 0) (y 0)
         (dx 0) (dy 1)
         (steps 0))
    (loop for i from 0 below (length (aref grid 0))
          when (char= (char (aref grid 0) i) #\|)
            do (setf x i) (loop-finish))
    (loop
      (let* ((cell (char (aref grid y) x)))
        (cond
          ((char= cell #\Space) (return-from solve (print steps)))
          ((char= cell #\+)
           (cond
             ((= dx 0)
              (if (and (> x 0) (char= (char (aref grid y) (- x 1)) #\-))
                  (setf dx -1 dy 0)
                  (setf dx 1 dy 0)))
             ((/= dx 0)
              (if (and (> y 0) (char= (char (aref grid (- y 1)) x) #\|))
                  (setf dx 0 dy -1)
                  (setf dx 0 dy 1)))))))
      (incf x dx)
      (incf y dy)
      (incf steps))))

(defun main ()
  (solve))

(main)
