
(defun read-heightmap (filename)
  (with-open-file (in filename :direction :input)
    (let* ((lines (loop for line = (read-line in nil nil)
                        while line
                        collect line))
           (rows (length lines))
           (cols (if (null lines) 0 (length (car lines))))
           (heightmap (make-array (list rows cols) :element-type 'fixnum)))
      (loop for r from 0 below rows
            for line = (nth r lines)
            do (loop for c from 0 below cols
                     do (setf (aref heightmap r c) (- (char-code (char line c)) (char-code #\0)))))
      heightmap)))

(defun solve ()
  (let* ((heightmap (read-heightmap "input.txt"))
         (rows (array-dimension heightmap 0))
         (cols (array-dimension heightmap 1))
         (total-risk-level 0))
    (loop for y from 0 below rows
          do (loop for x from 0 below cols
                   do (let ((height (aref heightmap y x)))
                        (when (and
                                (or (= x 0) (< height (aref heightmap y (- x 1))))
                                (or (= x (- cols 1)) (< height (aref heightmap y (+ x 1))))
                                (or (= y 0) (< height (aref heightmap (- y 1) x)))
                                (or (= y (- rows 1)) (< height (aref heightmap (+ y 1) x))))
                          (incf total-risk-level (1+ height))))))
    total-risk-level))

(defun main ()
  (print (solve)))

(main)
