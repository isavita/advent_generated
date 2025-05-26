
(defun get-next-state (grid r c rows cols)
  (let ((on-neighbors 0)
        (current-state (aref grid r c)))
    (loop for x from (max 0 (- r 1)) to (min (- rows 1) (+ r 1))
          do (loop for y from (max 0 (- c 1)) to (min (- cols 1) (+ c 1))
                   do (unless (and (= x r) (= y c))
                        (when (char= (aref grid x y) #\#)
                          (incf on-neighbors)))))
    (cond ((and (char= current-state #\#) (not (or (= on-neighbors 2) (= on-neighbors 3))))
           #\.)
          ((and (char= current-state #\.) (= on-neighbors 3))
           #\#)
          (t
           current-state))))

(defun animate (grid rows cols)
  (let ((new-grid (make-array (list rows cols) :element-type 'character)))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   do (setf (aref new-grid r c) (get-next-state grid r c rows cols))))
    new-grid))

(defun main ()
  (let* (lines
         rows
         cols
         grid)
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (push (coerce line 'list) lines)))
    (setf lines (nreverse lines))
    (setf rows (length lines))
    (setf cols (length (first lines)))

    (setf grid (make-array (list rows cols) :element-type 'character))
    (loop for r from 0 below rows
          for char-list in lines
          do (loop for c from 0 below cols
                   for char in char-list
                   do (setf (aref grid r c) char)))

    (loop repeat 100
          do (setf grid (animate grid rows cols)))

    (let ((result 0))
      (loop for r from 0 below rows
            do (loop for c from 0 below cols
                     when (char= (aref grid r c) #\#)
                       do (incf result)))
      (format t "~a~%" result))))

(main)
