
(defconstant +grid-size+ 100)
(defconstant +steps+ 100)

(defun count-on-neighbors (grid x y)
  (loop with on = 0
        for dx from -1 to 1
        do (loop for dy from -1 to 1
                 do (unless (and (= dx 0) (= dy 0))
                      (let ((nx (+ x dx))
                            (ny (+ y dy)))
                        (when (and (>= nx 0) (< nx +grid-size+)
                                   (>= ny 0) (< ny +grid-size+)
                                   (aref grid nx ny))
                          (incf on)))))
        finally (return on)))

(defun step-grid (grid)
  (let ((new-grid (make-array `(,+grid-size+ ,+grid-size+) :initial-element nil)))
    (loop for x from 0 below +grid-size+
          do (loop for y from 0 below +grid-size+
                   do (let ((on-neighbors (count-on-neighbors grid x y)))
                        (if (aref grid x y)
                            (setf (aref new-grid x y) (or (= on-neighbors 2) (= on-neighbors 3)))
                            (setf (aref new-grid x y) (= on-neighbors 3))))))
    (setf (aref new-grid 0 0) t)
    (setf (aref new-grid 0 (- +grid-size+ 1)) t)
    (setf (aref new-grid (- +grid-size+ 1) 0) t)
    (setf (aref new-grid (- +grid-size+ 1) (- +grid-size+ 1)) t)
    new-grid))

(defun main ()
  (let ((grid (make-array `(,+grid-size+ ,+grid-size+) :initial-element nil)))
    (with-open-file (file "input.txt" :direction :input)
      (loop for y from 0 below +grid-size+
            for line = (read-line file nil :eof)
            while (not (eq line :eof))
            do (loop for x from 0 below +grid-size+
                     for char = (char line x)
                     do (when (char= char #\#)
                          (setf (aref grid x y) t)))))
    (setf (aref grid 0 0) t)
    (setf (aref grid 0 (- +grid-size+ 1)) t)
    (setf (aref grid (- +grid-size+ 1) 0) t)
    (setf (aref grid (- +grid-size+ 1) (- +grid-size+ 1)) t)
    (loop for i from 1 to +steps+
          do (setf grid (step-grid grid)))
    (let ((on-count 0))
      (loop for x from 0 below +grid-size+
            do (loop for y from 0 below +grid-size+
                     do (when (aref grid x y)
                          (incf on-count))))
      (format t "~a~%" on-count))))

(main)
