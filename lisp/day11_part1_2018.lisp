
(defun calculate-power-level (x y serial)
  (let* ((rack-id (+ x 11))
         (power-level (* rack-id (+ y 1))))
    (incf power-level serial)
    (setf power-level (* power-level rack-id))
    (setf power-level (mod (floor power-level 100) 10))
    (- power-level 5)))

(defun main ()
  (let* ((serial (with-open-file (f "input.txt" :direction :input)
                   (parse-integer (read-line f))))
         (grid-size 300)
         (grid (make-array (list grid-size grid-size) :initial-element 0))
         (max-power most-negative-fixnum)
         (max-x 0)
         (max-y 0))
    (loop for y from 0 below grid-size
          do (loop for x from 0 below grid-size
                   do (setf (aref grid y x)
                            (calculate-power-level x y serial))))
    (loop for y from 0 to (- grid-size 3)
          do (loop for x from 0 to (- grid-size 3)
                   do (let ((current-total-power 0))
                        (loop for dy from 0 to 2
                              do (loop for dx from 0 to 2
                                       do (incf current-total-power (aref grid (+ y dy) (+ x dx)))))
                        (when (> current-total-power max-power)
                          (setf max-power current-total-power)
                          (setf max-x (+ x 1))
                          (setf max-y (+ y 1))))))
    (format t "~a,~a~%" max-x max-y)))

(main)
