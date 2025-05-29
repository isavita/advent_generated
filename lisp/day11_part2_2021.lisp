
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((lines (loop for line = (read-line stream nil nil)
                        while line
                        collect (loop for char across line
                                      collect (- (char-code char) (char-code #\0)))))
           (height (length lines))
           (width (length (first lines)))
           (grid (make-array (list height width) :element-type 'fixnum)))
      (loop for y from 0 below height
            for row in lines
            do (loop for x from 0 below width
                     for val in row
                     do (setf (aref grid y x) (the fixnum val))))
      grid)))

(defun flash (grid x y flashed height width)
  (declare (type (array fixnum (* *)) grid)
           (type fixnum x y height width)
           (type hash-table flashed))
  (let ((coords (list x y)))
    (when (gethash coords flashed)
      (return-from flash 0))

    (setf (gethash coords flashed) t)
    (let ((total-flashes 1)
          (directions '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))
      (dolist (dir directions)
        (let* ((dx (the fixnum (first dir)))
               (dy (the fixnum (second dir)))
               (new-x (the fixnum (+ x dx)))
               (new-y (the fixnum (+ y dy))))
          (when (and (>= new-x 0) (< new-x width)
                     (>= new-y 0) (< new-y height))
            (incf (the fixnum (aref grid new-y new-x)))
            (when (> (the fixnum (aref grid new-y new-x)) 9)
              (incf total-flashes (flash grid new-x new-y flashed height width))))))
      (the fixnum total-flashes))))

(defun simulate-step (grid height width)
  (declare (type (array fixnum (* *)) grid)
           (type fixnum height width))
  (let ((total-flashes 0)
        (flashed (make-hash-table :test 'equal)))

    (loop for y from 0 below height
          do (loop for x from 0 below width
                   do (incf (the fixnum (aref grid y x)))))

    (loop for y from 0 below height
          do (loop for x from 0 below width
                   when (> (the fixnum (aref grid y x)) 9)
                   do (incf total-flashes (flash grid x y flashed height width))))

    (maphash (lambda (coords value)
               (declare (ignore value))
               (setf (aref grid (the fixnum (second coords)) (the fixnum (first coords))) (the fixnum 0)))
             flashed)
    (the fixnum total-flashes)))

(defun main ()
  (let* ((grid (read-input "input.txt"))
         (height (the fixnum (array-dimension grid 0)))
         (width (the fixnum (array-dimension grid 1)))
         (step 0))
    (loop
      (incf step)
      (let ((flashes (simulate-step grid height width)))
        (when (= (the fixnum flashes) (the fixnum (* height width)))
          (format t "~A~%" step)
          (return))))))

(main)
