
(defstruct coord x y)

(defstruct grid width height data)

(defconstant +empty-char+ #\.)

(defun build-grid (input-lines)
  (let* ((height (length input-lines))
         (width (if (> height 0) (length (car input-lines)) 0))
         (data (make-hash-table :test 'equalp)))
    (loop for y from 0 below height
          for line in input-lines
          do (loop for x from 0 below width
                   for char = (char line x)
                   when (char/= char +empty-char+)
                     do (setf (gethash (make-coord :x x :y y) data) char)))
    (make-grid :width width :height height :data data)))

(defun get-empty-rows (grid)
  (let ((empty-rows '()))
    (loop for y from 0 below (grid-height grid)
          for is-empty = t
          do (loop for x from 0 below (grid-width grid)
                   do (when (gethash (make-coord :x x :y y) (grid-data grid))
                        (setf is-empty nil)
                        (return)))
          when is-empty
            do (push y empty-rows))
    (nreverse empty-rows)))

(defun get-empty-cols (grid)
  (let ((empty-cols '()))
    (loop for x from 0 below (grid-width grid)
          for is-empty = t
          do (loop for y from 0 below (grid-height grid)
                   do (when (gethash (make-coord :x x :y y) (grid-data grid))
                        (setf is-empty nil)
                        (return)))
          when is-empty
            do (push x empty-cols))
    (nreverse empty-cols)))

(defun calculate-offsets (empty-indexes bound)
  (let ((offsets (make-array bound :initial-element 0)))
    (loop with current-offset = 0
          with empty-idx-ptr = 0
          for i from 0 below bound
          do (setf (aref offsets i) current-offset)
          when (and (< empty-idx-ptr (length empty-indexes))
                    (= i (nth empty-idx-ptr empty-indexes)))
            do (incf current-offset)
               (incf empty-idx-ptr))
    offsets))

(defun calculate-optimized-length (c1 c2 dx dy num-lines-to-add)
  (let* ((x1 (coord-x c1))
         (y1 (coord-y c1))
         (x2 (coord-x c2))
         (y2 (coord-y c2))
         (min-x (min x1 x2))
         (max-x (max x1 x2))
         (min-y (min y1 y2))
         (max-y (max y1 y2))
         (original-dx (abs (- x2 x1)))
         (original-dy (abs (- y2 y1)))
         (empty-cols-between (- (aref dx max-x) (aref dx min-x)))
         (empty-rows-between (- (aref dy max-y) (aref dy min-y))))
    (+ original-dx
       (* empty-cols-between num-lines-to-add)
       original-dy
       (* empty-rows-between num-lines-to-add))))

(defun solve (input-lines expansion-factor)
  (let* ((original-grid (build-grid input-lines))
         (galaxy-coords (loop for key being the hash-keys of (grid-data original-grid) collect key))
         (empty-cols (get-empty-cols original-grid))
         (empty-rows (get-empty-rows original-grid))
         (num-lines-to-add (- expansion-factor 1))
         (dx (calculate-offsets empty-cols (grid-width original-grid)))
         (dy (calculate-offsets empty-rows (grid-height original-grid)))
         (total-dist 0))
    (loop for i from 0 below (length galaxy-coords)
          for c1 = (nth i galaxy-coords)
          do (loop for j from (1+ i) below (length galaxy-coords)
                   for c2 = (nth j galaxy-coords)
                   do (incf total-dist (calculate-optimized-length c1 c2 dx dy num-lines-to-add))))
    total-dist))

(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun main ()
  (let ((input-lines (read-file-lines "input.txt")))
    (print (solve input-lines 1000000))))

(eval-when (:load-toplevel :execute)
  (main))
