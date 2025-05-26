
(defstruct point x y vx vy)

(defun parse-line (line)
  (with-input-from-string (s (substitute #\Space #\= (substitute #\Space #\, (substitute #\Space #\< (substitute #\Space #\> line)))))
    (read s)
    (let ((x (read s))
          (y (read s)))
      (read s)
      (let ((vx (read s))
            (vy (read s)))
        (make-point :x x :y y :vx vx :vy vy)))))

(defun read-input (file-path)
  (with-open-file (f file-path :direction :input)
    (loop for line = (read-line f nil nil)
          while line
          collect (parse-line line))))

(defun simulate (points)
  (let ((seconds 0)
        (prev-area nil)
        (prev-points-positions nil))
    (loop
      (let ((min-x most-positive-fixnum)
            (max-x most-negative-fixnum)
            (min-y most-positive-fixnum)
            (max-y most-negative-fixnum)
            (current-positions ()))
        (dolist (p points)
          (let ((px (point-x p))
                (py (point-y p)))
            (setf min-x (min min-x px))
            (setf max-x (max max-x px))
            (setf min-y (min min-y py))
            (setf max-y (max max-y py))
            (push (list px py) current-positions)))
        (setf current-positions (nreverse current-positions))

        (let ((current-area (* (- max-x min-x) (- max-y min-y))))
          (when (and (plusp seconds) prev-area (> current-area prev-area))
            (return (values (- seconds 1) prev-points-positions)))
          (setf prev-area current-area)
          (setf prev-points-positions current-positions)))

      (dolist (p points)
        (incf (point-x p) (point-vx p))
        (incf (point-y p) (point-vy p)))

      (incf seconds))))

(defun print-message (points second)
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum)
        (min-y most-positive-fixnum)
        (max-y most-negative-fixnum))
    (dolist (p points)
      (let ((px (car p))
            (py (cadr p)))
        (setf min-x (min min-x px))
        (setf max-x (max max-x px))
        (setf min-y (min min-y py))
        (setf max-y (max max-y py))))

    (let* ((width (1+ (- max-x min-x)))
           (height (1+ (- max-y min-y)))
           (sky (make-array (list height width) :initial-element #\.)))
      (dolist (p points)
        (setf (aref sky (- (cadr p) min-y) (- (car p) min-x)) #\#))

      (format t "After ~D seconds:~%" second)
      (loop for r from 0 below height
            do (loop for c from 0 below width
                     do (princ (aref sky r c)))
               (princ #\Newline)))))

(defun main ()
  (multiple-value-bind (second message-points)
      (simulate (read-input "input.txt"))
    (print-message message-points second)))

(main)
