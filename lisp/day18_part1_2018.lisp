
(defconstant +input-file+ "input.txt")
(defconstant +width+ 50)
(defconstant +height+ 50)

(defun read-input (filename)
  "Reads the input file into a 2D character array."
  (let ((grid (make-array (list +height+ +width+) :element-type 'character)))
    (with-open-file (in filename)
      (loop for y from 0 below +height+
            for line = (read-line in nil nil)
            do (loop for x from 0 below +width+
                     do (setf (aref grid y x) (char line x)))))
    grid))

(defconstant +directions+
  '((-1 -1) (-1 0) (-1 1)
    (0 -1)         (0 1)
    (1 -1) (1 0) (1 1)))

(defun count-adjacent (grid x y)
  "Counts adjacent open, trees, and lumberyards."
  (let ((open 0) (trees 0) (lumberyards 0))
    (loop for (dx dy) in +directions+
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (and (>= nx 0) (< nx +width+)
                    (>= ny 0) (< ny +height+))
            do (case (aref grid ny nx)
                 (#\. (incf open))
                 (#\| (incf trees))
                 (#\# (incf lumberyards))))
    (values open trees lumberyards)))

(defun next-state (grid)
  "Calculates the next state of the grid."
  (let ((new-grid (make-array (list +height+ +width+) :element-type 'character)))
    (loop for y from 0 below +height+
          do (loop for x from 0 below +width+
                   for current = (aref grid y x)
                   do (multiple-value-bind (open trees lumberyards)
                          (count-adjacent grid x y)
                        (setf (aref new-grid y x)
                              (cond ((and (char= current #\.) (>= trees 3)) #\|)
                                    ((and (char= current #\|) (>= lumberyards 3)) #\#)
                                    ((and (char= current #\#) (or (= lumberyards 0) (= trees 0))) #\.)
                                    (t current))))))
    new-grid))

(defun resource-value (grid)
  "Calculates the resource value of the grid."
  (let ((trees 0) (lumberyards 0))
    (loop for y from 0 below +height+
          do (loop for x from 0 below +width+
                   do (case (aref grid y x)
                        (#\| (incf trees))
                        (#\# (incf lumberyards)))))
    (* trees lumberyards)))

(defun main ()
  "Main entry point of the program."
  (let ((grid (read-input +input-file+)))
    (loop for minute from 0 below 10
          do (setf grid (next-state grid)))
    (format t "~a~%" (resource-value grid))))

(main)
