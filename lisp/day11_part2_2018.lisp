
(defun calculate-power-level (x y serial-number)
  (let* ((rack-id (+ x 10))
         (power-level (* rack-id y)))
    (incf power-level serial-number)
    (setf power-level (* power-level rack-id))
    (setf power-level (mod (floor power-level 100) 10))
    (- power-level 5)))

(defun calculate-cumulative-grid (grid)
  (let* ((grid-dim (array-dimension grid 0))
         (cumulative-grid-dim (+ grid-dim 1))
         (cumulative-grid (make-array (list cumulative-grid-dim cumulative-grid-dim) :initial-element 0)))
    (loop for x from 1 below cumulative-grid-dim
          do (loop for y from 1 below cumulative-grid-dim
                   do (setf (aref cumulative-grid x y)
                            (+ (aref grid (- x 1) (- y 1))
                               (aref cumulative-grid (- x 1) y)
                               (aref cumulative-grid x (- y 1))
                               (- (aref cumulative-grid (- x 1) (- y 1)))))))
    cumulative-grid))

(defun calculate-total-power (cumulative-grid x y size)
  (let* ((x1 (- x 1))
         (y1 (- y 1))
         (x2 (+ x size -1))
         (y2 (+ y size -1)))
    (- (+ (aref cumulative-grid x2 y2)
          (aref cumulative-grid x1 y1))
       (aref cumulative-grid x1 y2)
       (aref cumulative-grid x2 y1))))

(defun find-largest-square (grid cumulative-grid)
  (let ((max-power most-negative-fixnum)
        (max-coordinates nil))
    (loop for size from 1 to 300
          do (loop for x from 1 to (- 301 size)
                   do (loop for y from 1 to (- 301 size)
                            do (let ((total-power (calculate-total-power cumulative-grid x y size)))
                                 (when (> total-power max-power)
                                   (setf max-power total-power)
                                   (setf max-coordinates (list x y size)))))))
    max-coordinates))

(defun main ()
  (let* ((serial-number 0)
         (grid (make-array '(300 300) :initial-element 0))
         (cumulative-grid nil)
         (max-coordinates nil))
    (with-open-file (file "input.txt" :direction :input)
      (setf serial-number (parse-integer (read-line file))))
    (loop for x from 0 below 300
          do (loop for y from 0 below 300
                   do (setf (aref grid x y) (calculate-power-level (+ x 1) (+ y 1) serial-number))))
    (setf cumulative-grid (calculate-cumulative-grid grid))
    (setf max-coordinates (find-largest-square grid cumulative-grid))
    (format t "~a,~a,~a~%" (first max-coordinates) (second max-coordinates) (third max-coordinates))))

(main)
