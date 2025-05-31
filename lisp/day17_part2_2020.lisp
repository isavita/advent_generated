
(defun get-input-path ()
  (merge-pathnames "input.txt" *load-pathname*))

(defun solve ()
  (let ((active-cubes (make-hash-table :test 'equal)))

    ;; Initialize active-cubes from input.txt
    (with-open-file (in (get-input-path))
      (loop for y from 0
            for line = (read-line in nil nil)
            while line
            do (loop for x from 0
                     for char across line
                     when (char= char #\#)
                       do (setf (gethash (list x y 0 0) active-cubes) t))))

    ;; Pre-compute neighbor deltas (3^4 - 1 = 80 deltas)
    (let ((neighbor-deltas
            (loop for dw from -1 to 1
                  nconc (loop for dz from -1 to 1
                              nconc (loop for dy from -1 to 1
                                          nconc (loop for dx from -1 to 1
                                                      unless (and (zerop dx) (zerop dy) (zerop dz) (zerop dw))
                                                        collect (list dx dy dz dw)))))))

      ;; Simulation cycles
      (loop for cycle from 1 to 6
            do (let ((new-active-cubes (make-hash-table :test 'equal))
                     (neighbor-counts (make-hash-table :test 'equal)))

                 ;; Populate neighbor-counts for all potential cubes
                 (maphash (lambda (coord _)
                            (destructuring-bind (x y z w) coord
                              (dolist (delta neighbor-deltas)
                                (destructuring-bind (dx dy dz dw) delta
                                  (let ((neighbor-coord (list (+ x dx) (+ y dy) (+ z dz) (+ w dw))))
                                    (incf (gethash neighbor-coord neighbor-counts 0)))))))
                          active-cubes)

                 ;; Determine new active cubes based on rules
                 (maphash (lambda (coord count)
                            (when (or (= count 3)
                                      (and (= count 2) (gethash coord active-cubes)))
                              (setf (gethash coord new-active-cubes) t)))
                          neighbor-counts)

                 ;; Update active-cubes for the next cycle
                 (setf active-cubes new-active-cubes)))

      ;; Print the final count of active cubes
      (format t "~A~%" (hash-table-count active-cubes)))))

(defun main ()
  (solve))

(main)
