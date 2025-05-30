
(defconstant +dirs+ '((0 -1) (1 0) (0 1) (-1 0)))

(defun solve ()
  (let* ((grid-lines (with-open-file (f "input.txt" :direction :input)
                       (loop for line = (read-line f nil)
                             while line
                             collect (coerce line 'list))))
         (h (length grid-lines))
         (w (length (first grid-lines)))
         (grid (make-array (list h w) :element-type 'character :initial-contents grid-lines)))
    (let ((x 0) (y 0) (dir-idx 0)
          (found-start nil))
      (loop for i from 0 below h
            until found-start
            do (loop for j from 0 below w
                     until found-start
                     do (let ((char (aref grid i j)))
                          (case char
                            (#\^ (setf x j y i dir-idx 0 found-start t))
                            (#\> (setf x j y i dir-idx 1 found-start t))
                            (#\v (setf x j y i dir-idx 2 found-start t))
                            (#\< (setf x j y i dir-idx 3 found-start t))))))
      (let ((visited (make-hash-table :test 'equal)))
        (setf (gethash (format nil "~a,~a" x y) visited) t)
        (loop named simulation-loop
              do (let* ((dx (first (nth dir-idx +dirs+)))
                        (dy (second (nth dir-idx +dirs+)))
                        (nx (+ x dx))
                        (ny (+ y dy)))
                   (when (or (< nx 0) (>= nx w) (< ny 0) (>= ny h))
                     (return-from simulation-loop))
                   (if (char= (aref grid ny nx) #\#)
                       (setf dir-idx (mod (+ dir-idx 1) 4))
                       (progn
                         (setf x nx y ny)
                         (setf (gethash (format nil "~a,~a" x y) visited) t)))))
        (print (hash-table-count visited))))))

(defun main ()
  (solve))

(main)
