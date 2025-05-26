
(defvar *data* nil)

(defun read-input (filename)
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil nil)
          while line
          collect line)))

(defun count-trees (right down map-data)
  (let ((trees 0)
        (col 0)
        (map-width (length (first map-data))))
    (loop with current-map-position = map-data
          for current-row-string = (car current-map-position)
          while current-row-string
          do
             (when (char= #\# (char current-row-string (mod col map-width)))
               (incf trees))
             (incf col right)
             (loop for i from 0 below down
                   do
                      (setf current-map-position (cdr current-map-position))
                      (unless current-map-position (return))))
    trees))

(defun main ()
  (setf *data* (read-input "input.txt"))
  (let* ((s1 (count-trees 1 1 *data*))
         (s2 (count-trees 3 1 *data*))
         (s3 (count-trees 5 1 *data*))
         (s4 (count-trees 7 1 *data*))
         (s5 (count-trees 1 2 *data*)))
    (print (* s1 s2 s3 s4 s5))))

(main)
