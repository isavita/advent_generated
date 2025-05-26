
(defun read-grid (filename)
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil nil)
          while line
          collect line)))

(defun valid-horizontal-segment-p (c)
  (or (char= (char-downcase c) #\-)
      (and (alpha-char-p c) (upper-case-p c))))

(defun valid-vertical-segment-p (c)
  (or (char= c #\|)
      (and (alpha-char-p c) (upper-case-p c))))

(defun main ()
  (let* ((grid (coerce (read-grid "input.txt") 'vector))
         (height (length grid))
         (width (length (svref grid 0)))
         (x 0)
         (y 0)
         (dx 0)
         (dy 1)
         (letters '()))
    (loop for i from 0 below width
          when (char= (char (svref grid 0) i) #\|)
            do (setf x i) (return))
    (loop
      (unless (and (>= x 0) (< x width) (>= y 0) (< y height)) (return))
      (let ((cell (char (svref grid y) x)))
        (when (char= cell #\Space) (return))
        (when (alpha-char-p cell) (push cell letters))
        (when (char= cell #\+)
          (if (zerop dx)
              (if (and (> x 0) (valid-horizontal-segment-p (char (svref grid y) (- x 1))))
                  (setf dx -1 dy 0)
                  (setf dx 1 dy 0))
              (if (and (> y 0) (valid-vertical-segment-p (char (svref grid (- y 1)) x)))
                  (setf dx 0 dy -1)
                  (setf dx 0 dy 1)))))
      (incf x dx)
      (incf y dy))
    (format t "~{~C~}~%" (nreverse letters))))

(main)
