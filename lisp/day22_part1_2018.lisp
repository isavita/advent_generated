
(defun parse-input (data)
  (let* ((line-pos1 (position #\Newline data))
         (line1 (subseq data 0 line-pos1))
         (line2 (subseq data (1+ line-pos1)))
         (depth-str (subseq line1 (1+ (position #\Space line1 :from-end t))))
         (depth (parse-integer depth-str))
         (coords-str (subseq line2 (1+ (position #\Space line2 :from-end t))))
         (comma-pos (position #\, coords-str))
         (x-str (subseq coords-str 0 comma-pos))
         (y-str (subseq coords-str (1+ comma-pos)))
         (x (parse-integer x-str))
         (y (parse-integer y-str)))
    (values depth (vector x y))))

(defun make-cave-system (depth target)
  (let* ((target-x (aref target 0))
         (target-y (aref target 1))
         (max-x (+ target-x 1))
         (max-y (+ target-y 1))
         (cave (make-array (list max-y max-x) :initial-element 0)))
    (loop for y from 0 below max-y do
      (loop for x from 0 below max-x do
        (let ((geologic-index 0))
          (cond
            ((or (and (= x 0) (= y 0)) (and (= x target-x) (= y target-y)))
             (setf geologic-index 0))
            ((= y 0)
             (setf geologic-index (* x 16807)))
            ((= x 0)
             (setf geologic-index (* y 48271)))
            (t
             (setf geologic-index (* (aref cave y (- x 1)) (aref cave (- y 1) x)))))
          (setf (aref cave y x) (mod (+ geologic-index depth) 20183)))))
    cave))

(defun calculate-risk-level (cave target)
  (let* ((target-x (aref target 0))
         (target-y (aref target 1))
         (max-x (+ target-x 1))
         (max-y (+ target-y 1))
         (risk-level 0))
    (loop for y from 0 below max-y do
      (loop for x from 0 below max-x do
        (incf risk-level (mod (aref cave y x) 3))))
    risk-level))

(defun main ()
  (let ((data (with-open-file (f "input.txt" :direction :input :if-does-not-exist :error)
                (let ((s (make-string (file-length f))))
                  (read-sequence s f)
                  s))))
    (multiple-value-bind (depth target) (parse-input data)
      (let* ((cave (make-cave-system depth target))
             (risk-level (calculate-risk-level cave target)))
        (format t "~a~%" risk-level)))))

(main)
