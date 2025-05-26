
(defun read-grid (filename)
  (with-open-file (f filename :direction :input)
    (coerce (loop for line = (read-line f nil nil)
                  while line
                  collect line)
            'vector)))

(defun check (s)
  (or (string= s "MAS") (string= s "SAM")))

(defun main ()
  (let* ((grid (read-grid "input.txt"))
         (r (length grid))
         (c (length (aref grid 0)))
         (count 0))
    (loop for x from 1 to (- r 2)
          for row-x = (aref grid x)
          for row-x-1 = (aref grid (- x 1))
          for row-x+1 = (aref grid (+ x 1))
          do
      (loop for y from 1 to (- c 2) do
        (when (char= (char row-x y) #\A)
          (let* ((char-x-y (char row-x y))
                 (char-x-1-y-1 (char row-x-1 (- y 1)))
                 (char-x-1-y+1 (char row-x-1 (+ y 1)))
                 (char-x+1-y-1 (char row-x+1 (- y 1)))
                 (char-x+1-y+1 (char row-x+1 (+ y 1)))
                 (d1 (format nil "~a~a~a" char-x-1-y-1 char-x-y char-x+1-y+1))
                 (d2 (format nil "~a~a~a" char-x-1-y+1 char-x-y char-x+1-y-1)))
            (when (and (check d1) (check d2))
              (incf count))))))
    (format t "~a~%" count)))

(main)
