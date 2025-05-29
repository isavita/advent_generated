
(defun read-input (file-path)
  (with-open-file (stream file-path :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-integer line))))

(defun quantum-entanglement (group)
  (reduce #'* group :initial-value 1))

(defun find-optimal-grouping (weights num-groups)
  (let* ((total-weight (reduce #'+ weights))
         (target-weight (floor total-weight num-groups)))
    (unless (= (* target-weight num-groups) total-weight)
      (return-from find-optimal-grouping nil))

    (let* ((sorted-weights (sort (copy-list weights) #'>))
           (min-group-size most-positive-fixnum)
           (min-qe most-positive-fixnum))

      (labels ((find-group (remaining-weights current-weight current-group-items)
                 (cond
                   ((= current-weight target-weight)
                    (let ((group-size (length current-group-items))
                          (current-qe (quantum-entanglement current-group-items)))
                      (when (< group-size min-group-size)
                        (setf min-group-size group-size
                              min-qe current-qe))
                      (when (and (= group-size min-group-size)
                                 (< current-qe min-qe))
                        (setf min-qe current-qe))))
                   ((or (> current-weight target-weight)
                        (null remaining-weights)
                        (>= (length current-group-items) min-group-size)))
                   (t
                    (let* ((current-weight-item (car remaining-weights))
                           (rest-weights (cdr remaining-weights)))
                      (when (<= (+ current-weight current-weight-item) target-weight)
                        (find-group rest-weights
                                    (+ current-weight current-weight-item)
                                    (cons current-weight-item current-group-items)))
                      (find-group rest-weights
                                  current-weight
                                  current-group-items))))))
        (find-group sorted-weights 0 '())
        (if (= min-qe most-positive-fixnum) nil min-qe)))))

(defun main ()
  (let* ((weights (read-input "input.txt"))
         (part1-result (find-optimal-grouping weights 3))
         (part2-result (find-optimal-grouping weights 4)))
    (format t "Part 1: ~a~%" part1-result)
    (format t "Part 2: ~a~%" part2-result)))

(main)
