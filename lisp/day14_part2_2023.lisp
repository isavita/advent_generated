
(defstruct (coord (:constructor make-coord (x y))) x y)

(defun coord-add (c1 c2)
  (make-coord (+ (coord-x c1) (coord-x c2)) (+ (coord-y c1) (coord-y c2))))

(defun coord-in-bounds-p (coord grid-width grid-height)
  (and (>= (coord-x coord) 0) (< (coord-x coord) grid-width)
       (>= (coord-y coord) 0) (< (coord-y coord) grid-height)))

(defstruct (grid (:constructor make-grid (width height data))) width height data)

(defun copy-hash-table (original-ht)
  (let ((new-ht (make-hash-table :test (hash-table-test original-ht))))
    (maphash (lambda (k v) (setf (gethash k new-ht) v)) original-ht)
    new-ht))

(defun make-grid-from-input (input-data)
  (let* ((height (length input-data))
         (width (length (car input-data)))
         (data (make-hash-table :test 'equalp)))
    (loop for y from 0 below height
          for line = (nth y input-data)
          do (loop for x from 0 below width
                   for char = (char line x)
                   when (char/= char #\.)
                   do (setf (gethash (make-coord x y) data) char)))
    (make-grid width height data)))

(defun shift-single-rock (grid coord direction)
  (when (and (gethash coord (grid-data grid))
             (char= (gethash coord (grid-data grid)) #\O))
    (let* ((current coord)
           (before (coord-add coord direction)))
      (loop while (and (coord-in-bounds-p before (grid-width grid) (grid-height grid))
                       (not (gethash before (grid-data grid))))
            do (setf (gethash before (grid-data grid)) #\O)
               (remhash current (grid-data grid))
               (setf current before)
               (setf before (coord-add before direction))))))

(defun shift-rocks (grid direction)
  (let ((width (grid-width grid))
        (height (grid-height grid)))
    (labels ((shift-rock-at (x y)
               (shift-single-rock grid (make-coord x y) direction)))
      (let ((north-dir (make-coord 0 -1))
            (west-dir (make-coord -1 0)))
        (cond ((or (equalp direction north-dir)
                   (equalp direction west-dir))
               (loop for y from 0 below height
                     do (loop for x from 0 below width
                              do (shift-rock-at x y))))
              (t
               (loop for y downfrom (1- height) to 0
                     do (loop for x downfrom (1- width) to 0
                              do (shift-rock-at x y)))))))))

(defun cycle-rocks (grid)
  (shift-rocks grid (make-coord 0 -1))
  (shift-rocks grid (make-coord -1 0))
  (shift-rocks grid (make-coord 0 1))
  (shift-rocks grid (make-coord 1 0)))

(defun calculate-grid-key (grid)
  (let ((key 0)
        (width (grid-width grid)))
    (maphash (lambda (coord char)
               (when (char= char #\O)
                 (incf key (+ (coord-x coord) (* (coord-y coord) width)))))
             (grid-data grid))
    key))

(defun calculate-load (grid)
  (let ((load 0)
        (height (grid-height grid)))
    (maphash (lambda (coord char)
               (when (char= char #\O)
                 (incf load (- height (coord-y coord)))))
             (grid-data grid))
    load))

(defun solve (input-data)
  (let* ((num-cycles 1000000000)
         (grid (make-grid-from-input input-data))
         (cache (make-hash-table)))
    (loop for i from 0 below num-cycles
          for grid-key = (calculate-grid-key grid)
          do (multiple-value-bind (value foundp) (gethash grid-key cache)
               (when foundp
                 (let* ((i-start-cycle value)
                        (cycle-length (- i i-start-cycle))
                        (remaining-cycles (mod (- num-cycles i-start-cycle) cycle-length)))
                   (loop repeat remaining-cycles
                         do (cycle-rocks grid))
                   (return-from solve (calculate-load grid))))
               (setf (gethash grid-key cache) i)
               (cycle-rocks grid)))
    (calculate-load grid)))

(defun read-file (filename)
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil nil)
          while line
          collect line)))

(defun main ()
  (let ((input-data (read-file "input.txt")))
    (print (solve input-data))))

(main)
