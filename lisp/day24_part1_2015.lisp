
(defun read-input (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-integer line))))

(defun find-quantum-entanglement (weights)
  (let* ((sorted-weights-vec (coerce (sort (copy-list weights) #'>) 'vector))
         (total-weight (reduce #'+ sorted-weights-vec))
         (target-weight (/ total-weight 3)))

    (unless (zerop (mod total-weight 3))
      (error "It is not possible to split the packages into three equal groups."))

    (let ((min-group-size most-positive-fixnum)
          (min-quantum-entanglement most-positive-fixnum))

      (labels ((find-groups (index current-sum group-size current-product)
                 (when (> group-size min-group-size)
                   (return-from find-groups))
                 (when (> current-sum target-weight)
                   (return-from find-groups))

                 (when (= current-sum target-weight)
                   (cond
                     ((< group-size min-group-size)
                      (setf min-group-size group-size)
                      (setf min-quantum-entanglement current-product))
                     ((and (= group-size min-group-size)
                           (< current-product min-quantum-entanglement))
                      (setf min-quantum-entanglement current-product)))
                   (return-from find-groups))

                 (when (>= index (length sorted-weights-vec))
                   (return-from find-groups))

                 (loop for i from index below (length sorted-weights-vec)
                       do (let* ((weight (aref sorted-weights-vec i))
                                 (new-sum (+ current-sum weight)))
                            (when (<= new-sum target-weight)
                              (find-groups (1+ i)
                                           new-sum
                                           (1+ group-size)
                                           (* current-product weight)))))))

        (find-groups 0 0 0 1)
        min-quantum-entanglement))))

(defun main ()
  (let* ((filepath "input.txt")
         (weights (read-input filepath))
         (quantum-entanglement (find-quantum-entanglement weights)))
    (format t "~a~%" quantum-entanglement)))

(main)
