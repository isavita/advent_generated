
(defconstant +open+ #\.)
(defconstant +trees+ #\|)
(defconstant +lumberyard+ #\#)

(defun read-input (filename)
  (let* ((lines (loop with file = (open filename :direction :input)
                      for line = (read-line file nil nil)
                      while line
                      collect line
                      finally (close file)))
         (rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type 'character)))
    (loop for r from 0 below rows do
      (let ((line (nth r lines)))
        (loop for c from 0 below cols do
          (setf (aref grid r c) (char line c)))))
    grid))

(defun count-adjacent (grid r c acre-type)
  (multiple-value-bind (rows cols) (values (array-dimension grid 0) (array-dimension grid 1))
    (let ((count 0))
      (loop for dr from -1 to 1 do
        (loop for dc from -1 to 1 do
          (unless (and (zerop dr) (zerop dc))
            (let ((nr (+ r dr))
                  (nc (+ c dc)))
              (when (and (>= nr 0) (< nr rows)
                         (>= nc 0) (< nc cols))
                (when (char= (aref grid nr nc) acre-type)
                  (incf count)))))))
      count)))

(defun next-acre-state (grid r c)
  (let ((acre (aref grid r c)))
    (cond
      ((char= acre +open+)
       (if (>= (count-adjacent grid r c +trees+) 3)
           +trees+
           +open+))
      ((char= acre +trees+)
       (if (>= (count-adjacent grid r c +lumberyard+) 3)
           +lumberyard+
           +trees+))
      ((char= acre +lumberyard+)
       (if (and (>= (count-adjacent grid r c +lumberyard+) 1)
                (>= (count-adjacent grid r c +trees+) 1))
           +lumberyard+
           +open+))
      (t acre))))

(defun transform (grid)
  (multiple-value-bind (rows cols) (values (array-dimension grid 0) (array-dimension grid 1))
    (let ((new-grid (make-array (list rows cols) :element-type 'character)))
      (loop for r from 0 below rows do
        (loop for c from 0 below cols do
          (setf (aref new-grid r c) (next-acre-state grid r c))))
      new-grid)))

(defun count-resources (grid)
  (multiple-value-bind (rows cols) (values (array-dimension grid 0) (array-dimension grid 1))
    (let ((wooded 0)
          (lumberyards 0))
      (loop for r from 0 below rows do
        (loop for c from 0 below cols do
          (let ((acre (aref grid r c)))
            (cond
              ((char= acre +trees+) (incf wooded))
              ((char= acre +lumberyard+) (incf lumberyards))))))
      (values wooded lumberyards))))

(defun grid-to-string (grid)
  (multiple-value-bind (rows cols) (values (array-dimension grid 0) (array-dimension grid 1))
    (format nil "~{~a~^~%~}"
            (loop for r from 0 below rows
                  collect (loop for c from 0 below cols
                                collect (aref grid r c) into row-chars
                                finally (return (coerce row-chars 'string)))))))

(defun main ()
  (let* ((grid (read-input "input.txt"))
         (seen-states (make-hash-table :test 'equal))
         (cycle-start 0)
         (cycle-length 0)
         (total-minutes 1000000000))

    (loop for minute from 0 below total-minutes
          for state = (grid-to-string grid)
          do (multiple-value-bind (found-minute present-p)
                 (gethash state seen-states)
               (if present-p
                   (progn
                     (setf cycle-start found-minute)
                     (setf cycle-length (- minute found-minute))
                     (return))
                   (setf (gethash state seen-states) minute)))
          (setf grid (transform grid)))

    (let* ((remaining-minutes (mod (- total-minutes cycle-start) cycle-length)))
      (loop repeat remaining-minutes do
        (setf grid (transform grid))))

    (multiple-value-bind (wooded lumberyards)
        (count-resources grid)
      (format t "~a~%" (* wooded lumberyards)))))

(main)
