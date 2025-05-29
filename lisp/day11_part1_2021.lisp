
(defconstant +directions+
  '((-1 -1) (-1 0) (-1 1)
    (0 -1)          (0 1)
    (1 -1) (1 0) (1 1)))

(defun read-input (filename)
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil nil)
          while line
          collect (loop for char across line
                        collect (- (char-code char) (char-code #\0))))))

(defun parse-grid (lines)
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type 'fixnum)))
    (loop for r from 0 below rows
          for row-list in lines
          do (loop for c from 0 below cols
                   for val in row-list
                   do (setf (aref grid r c) val)))
    grid))

(defun simulate-step (grid)
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (flashed (make-hash-table :test 'equal))
         (flash-count 0))

    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   do (incf (aref grid r c))))

    (labels ((flash-octopus (r c)
               (let ((key (list r c)))
                 (when (or (gethash key flashed)
                           (<= (aref grid r c) 9))
                   (return-from flash-octopus))

                 (setf (gethash key flashed) t)
                 (incf flash-count)

                 (dolist (dir +directions+)
                   (let* ((dr (first dir))
                          (dc (second dir))
                          (nr (+ r dr))
                          (nc (+ c dc)))
                     (when (and (>= nr 0) (< nr rows)
                                (>= nc 0) (< nc cols))
                       (incf (aref grid nr nc))
                       (flash-octopus nr nc)))))))

      (loop for r from 0 below rows
            do (loop for c from 0 below cols
                     when (> (aref grid r c) 9)
                       do (flash-octopus r c))))

    (maphash (lambda (key val)
               (declare (ignore val))
               (setf (aref grid (first key) (second key)) 0))
             flashed)

    flash-count))

(defun total-flashes-after-steps (grid steps)
  (loop with total-flashes = 0
        for i from 1 to steps
        do (incf total-flashes (simulate-step grid))
        finally (return total-flashes)))

(defun main ()
  (let* ((raw-lines (read-input "input.txt"))
         (grid (parse-grid raw-lines)))
    (print (total-flashes-after-steps grid 100))))

(main)
