
(defun get-neighbors (x y z)
  (loop for dz from -1 to 1
        nconc (loop for dy from -1 to 1
                      nconc (loop for dx from -1 to 1
                                  when (not (and (= dx 0) (= dy 0) (= dz 0)))
                                    collect (list (+ x dx) (+ y dy) (+ z dz))))))

(defun main ()
  (let* ((lines
           (with-open-file (f "input.txt" :direction :input)
             (loop for line = (read-line f nil)
                   while line
                   collect line)))
         (active-cubes (make-hash-table :test 'equal)))

    (loop for y from 0 below (length lines)
          do (loop for x from 0 below (length (car lines))
                   do (when (char= (char (nth y lines) x) #\#)
                        (setf (gethash (list x y 0) active-cubes) t))))

    (loop for cycle from 1 to 6
          do (let ((new-active-cubes (make-hash-table :test 'equal))
                   (inactive-neighbors (make-hash-table :test 'equal)))

               (maphash (lambda (cube active-p)
                          (declare (ignore active-p))
                          (destructuring-bind (cx cy cz) cube
                            (let ((count-active-neighbors 0))
                              (dolist (neighbor (get-neighbors cx cy cz))
                                (if (gethash neighbor active-cubes)
                                    (incf count-active-neighbors)
                                    (incf (gethash neighbor inactive-neighbors 0))))
                              (when (or (= count-active-neighbors 2)
                                        (= count-active-neighbors 3))
                                (setf (gethash cube new-active-cubes) t)))))
                        active-cubes)

               (maphash (lambda (inactive-cube count)
                          (when (= count 3)
                            (setf (gethash inactive-cube new-active-cubes) t)))
                        inactive-neighbors)

               (setf active-cubes new-active-cubes)))

    (format t "~a~%" (hash-table-count active-cubes))))

(main)
