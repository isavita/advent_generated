
(defconstant +side+ 5)
(defconstant +square+ (* +side+ +side+))

(defun parse-input (filename)
  (let ((res (make-array +square+ :initial-element nil)))
    (with-open-file (stream filename :direction :input)
      (loop for row from 0 below +side+
            for line = (read-line stream nil nil)
            while line
            do (loop for col from 0 below +side+
                     when (char= (char line col) #\#)
                       do (setf (aref res (+ (* row +side+) col)) t))))
    res))

(defun infested (space level cell)
  (let ((level-grid (gethash level space)))
    (and level-grid (aref level-grid cell))))

(defun clean-space (space)
  (let ((min-level most-positive-fixnum)
        (max-level most-negative-fixnum)
        (has-levels nil))
    (maphash (lambda (level grid)
               (declare (ignore grid))
               (setf min-level (min min-level level)
                     max-level (max max-level level)
                     has-levels t))
             space)

    (when has-levels
      (let ((min-bugs 0)
            (max-bugs 0))
        (loop for cell from 0 below +square+
              when (infested space min-level cell)
                do (incf min-bugs)
              when (infested space max-level cell)
                do (incf max-bugs))
        (when (= min-bugs 0)
          (remhash min-level space))
        (when (= max-bugs 0)
          (remhash max-level space))))))

(defun next-state (space)
  (let ((new-space (make-hash-table))
        (min-level most-positive-fixnum)
        (max-level most-negative-fixnum)
        (has-levels nil))

    (maphash (lambda (level grid)
               (declare (ignore grid))
               (setf min-level (min min-level level)
                     max-level (max max-level level)
                     has-levels t))
             space)

    (unless has-levels
      (setf min-level 0 max-level 0))

    (loop for level from (1- min-level) to (1+ max-level)
          do (let ((new-level-grid (make-array +square+ :initial-element nil)))
               (setf (gethash level new-space) new-level-grid)

               (loop for cell from 0 below +square+
                     when (/= cell 12)
                       do (let ((row (floor cell +side+))
                                (col (mod cell +side+))
                                (neighbours 0))

                            (when (and (= row 0) (infested space (1- level) 7)) (incf neighbours))
                            (when (and (= col 0) (infested space (1- level) 11)) (incf neighbours))
                            (when (and (= col 4) (infested space (1- level) 13)) (incf neighbours))
                            (when (and (= row 4) (infested space (1- level) 17)) (incf neighbours))

                            (when (= cell 7)
                              (loop for i from 0 below +side+
                                    when (infested space (1+ level) i)
                                      do (incf neighbours)))
                            (when (= cell 11)
                              (loop for i from 0 below +side+
                                    when (infested space (1+ level) (* 5 i))
                                      do (incf neighbours)))
                            (when (= cell 13)
                              (loop for i from 0 below +side+
                                    when (infested space (1+ level) (+ (* 5 i) (1- +side+)))
                                      do (incf neighbours)))
                            (when (= cell 17)
                              (loop for i from 0 below +side+
                                    when (infested space (1+ level) (+ (* (1- +side+) +side+) i))
                                      do (incf neighbours)))

                            (when (and (> row 0) (/= cell 17) (infested space level (- cell +side+))) (incf neighbours))
                            (when (and (> col 0) (/= cell 13) (infested space level (1- cell))) (incf neighbours))
                            (when (and (< col (1- +side+)) (/= cell 11) (infested space level (1+ cell))) (incf neighbours))
                            (when (and (< row (1- +side+)) (/= cell 7) (infested space level (+ cell +side+))) (incf neighbours))

                            (let ((current-infested (infested space level cell)))
                              (cond
                                ((and current-infested (/= neighbours 1))
                                 (setf (aref new-level-grid cell) nil))
                                ((and (not current-infested) (or (= neighbours 1) (= neighbours 2)))
                                 (setf (aref new-level-grid cell) t))
                                (t
                                 (setf (aref new-level-grid cell) current-infested))))))))
    (clean-space new-space)
    new-space))

(defun main ()
  (let* ((initial-grid (parse-input "input.txt"))
         (space (make-hash-table)))
    (setf (gethash 0 space) initial-grid)

    (loop for i from 0 below 200
          do (setf space (next-state space)))

    (let ((total-bugs 0))
      (maphash (lambda (level grid)
                 (declare (ignore level))
                 (loop for bug-state across grid
                       when bug-state
                         do (incf total-bugs)))
               space)
      (format t "~a~%" total-bugs))))

(main)
