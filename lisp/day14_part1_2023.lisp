
(defstruct grid
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (data (make-hash-table :test 'equal) :type hash-table))

(defun coord-add (c1 c2)
  (list (+ (first c1) (first c2)) (+ (second c1) (second c2))))

(defun is-in-bounds (coord grid)
  (let ((x (first coord))
        (y (second coord)))
    (and (>= x 0) (< x (grid-width grid))
         (>= y 0) (< y (grid-height grid)))))

(defun grid-init (input-data)
  (let* ((height (length input-data))
         (width (if (> height 0) (length (first input-data)) 0))
         (grid (make-grid :width width :height height)))
    (loop for y from 0 below height
          for line = (nth y input-data)
          do (loop for x from 0 below width
                   for char = (char line x)
                   when (char/= char #\.)
                   do (setf (gethash (list x y) (grid-data grid)) char)))
    grid))

(defun shift-single-rock (grid coord direction)
  (let ((data (grid-data grid)))
    (when (char= (gethash coord data #\.) #\O)
      (let ((current coord)
            (before (coord-add coord direction)))
        (loop while (and (is-in-bounds before grid)
                         (not (gethash before data)))
              do (setf (gethash before data) #\O)
                 (remhash current data)
                 (setf current before)
                 (setf before (coord-add before direction)))))))

(defun shift-rocks (grid direction)
  (let ((width (grid-width grid))
        (height (grid-height grid)))
    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (shift-single-rock grid (list x y) direction)))))

(defun calculate-load (grid)
  (let ((load 0)
        (height (grid-height grid))
        (data (grid-data grid)))
    (maphash (lambda (coord cell)
               (when (char= cell #\O)
                 (incf load (- height (second coord)))))
             data)
    load))

(defun solve (input-data)
  (let ((grid (grid-init input-data)))
    (shift-rocks grid (list 0 -1))
    (calculate-load grid)))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let ((input-data (loop for line = (read-line f nil)
                            while line
                            collect line)))
      (print (solve input-data)))))

(main)
