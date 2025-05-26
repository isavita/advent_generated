
(defconstant +word+ "XMAS")
(defconstant +word-length+ (length +word+))
(defconstant +dirs+ '((1 0) (-1 0) (0 1) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1)))

(defun read-grid (filename)
  (with-open-file (f filename)
    (loop for line = (read-line f nil nil)
          while line
          collect line)))

(defun main ()
  (let* ((grid (read-grid "input.txt"))
         (r (length grid))
         (c (length (first grid)))
         (count 0))
    (loop for i from 0 below r do
      (loop for j from 0 below c do
        (when (char= (char (elt grid i) j) (char +word+ 0))
          (loop for (dx dy) in +dirs+ do
            (let ((x i) (y j) (k 0))
              (loop
                (when (or (< x 0) (>= x r) (< y 0) (>= y c) (>= k +word-length+)
                          (char/= (char (elt grid x) y) (char +word+ k)))
                  (return))
                (incf x dx)
                (incf y dy)
                (incf k))
              (when (= k +word-length+)
                (incf count)))))))
    (format t "~a~%" count)))

(main)
