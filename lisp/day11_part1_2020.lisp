
(defun deep-copy-grid (grid)
  (loop for row in grid
        collect (copy-list row)))

(defun read-seating-area (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (coerce (string-trim '(#\Newline #\Return) line) 'list))))

(defun simulate-seating (initial-grid)
  (let* ((rows (length initial-grid))
         (cols (length (first initial-grid)))
         (directions '((-1 . -1) (-1 . 0) (-1 . 1)
                       ( 0 . -1)           ( 0 . 1)
                       ( 1 . -1) ( 1 . 0) ( 1 . 1)))
         (current-grid (deep-copy-grid initial-grid)))
    (loop
      (let* ((next-grid (deep-copy-grid current-grid))
             (changes 0))
        (loop for i from 0 below rows
              do (loop for j from 0 below cols
                       do (let ((current-char (elt (elt current-grid i) j)))
                            (when (char/= current-char #\.)
                              (let ((occupied-neighbors 0))
                                (loop for dir in directions
                                      do (let* ((dx (car dir))
                                                (dy (cdr dir))
                                                (nx (+ i dx))
                                                (ny (+ j dy)))
                                           (when (and (>= nx 0) (< nx rows)
                                                      (>= ny 0) (< ny cols)
                                                      (char= (elt (elt current-grid nx) ny) #\#))
                                             (incf occupied-neighbors))))
                                (cond
                                  ((and (char= current-char #\L) (= occupied-neighbors 0))
                                   (setf (elt (elt next-grid i) j) #\#)
                                   (incf changes))
                                  ((and (char= current-char #\#) (>= occupied-neighbors 4))
                                   (setf (elt (elt next-grid i) j) #\L)
                                   (incf changes))))))))
        (if (= changes 0)
            (return current-grid)
            (setf current-grid next-grid))))))

(defun count-total-occupied (grid)
  (loop for row in grid
        sum (count #\# row)))

(defun main ()
  (let* ((initial-seating-area (read-seating-area "input.txt"))
         (final-seating-area (simulate-seating initial-seating-area)))
    (print (count-total-occupied final-seating-area))))

(main)
