
(defconstant +width+ 101)
(defconstant +height+ 103)
(defconstant +steps+ 100)
(defconstant +center-x+ 50)
(defconstant +center-y+ 51)

(defun parse-robot-line (line)
  (let* ((space-pos (position #\Space line))
         (p-str (subseq line 0 space-pos))
         (v-str (subseq line (1+ space-pos)))
         (p-comma-pos (position #\, p-str :start 2))
         (v-comma-pos (position #\, v-str :start 2)))
    (list
     (parse-integer (subseq p-str 2 p-comma-pos))
     (parse-integer (subseq p-str (1+ p-comma-pos)))
     (parse-integer (subseq v-str 2 v-comma-pos))
     (parse-integer (subseq v-str (1+ v-comma-pos))))))

(defun main ()
  (let* ((robots (with-open-file (f "input.txt" :direction :input)
                   (loop for line = (read-line f nil)
                         while line
                         collect (parse-robot-line line))))
         (q1 0) (q2 0) (q3 0) (q4 0))

    (dotimes (_ +steps+)
      (dolist (r robots)
        (setf (nth 0 r) (mod (+ (nth 0 r) (nth 2 r)) +width+))
        (setf (nth 1 r) (mod (+ (nth 1 r) (nth 3 r)) +height+))))

    (dolist (r robots)
      (let ((x (nth 0 r))
            (y (nth 1 r)))
        (unless (or (= x +center-x+) (= y +center-y+))
          (cond
            ((and (< x +center-x+) (< y +center-y+)) (incf q1))
            ((and (> x +center-x+) (< y +center-y+)) (incf q2))
            ((and (< x +center-x+) (> y +center-y+)) (incf q3))
            ((and (> x +center-x+) (> y +center-y+)) (incf q4))))))

    (format t "~a~%" (* q1 q2 q3 q4))))

(main)
