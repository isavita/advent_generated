
(defun calculate-region (grid rows cols start-row start-col visited)
  (let* ((char-at-start (char (aref grid start-row) start-col))
         (area 0)
         (perimeter 0)
         (queue (list (list start-row start-col))))

    (setf (aref visited start-row start-col) t)

    (loop while queue do
      (let* ((current-point (pop queue))
             (r (first current-point))
             (c (second current-point)))
        (incf area)

        (let ((neighbors (list (list (- r 1) c)
                               (list (+ r 1) c)
                               (list r (- c 1))
                               (list r (+ c 1)))))
          
          (loop for neighbor-coords in neighbors do
            (destructuring-bind (nr nc) neighbor-coords
              (if (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))
                  (if (char/= (char (aref grid nr) nc) char-at-start)
                      (incf perimeter)
                      (unless (aref visited nr nc)
                        (setf (aref visited nr nc) t)
                        (setf queue (nconc queue (list neighbor-coords)))))
                  (incf perimeter)))))))
    (values area perimeter)))

(defun solve (grid-lines)
  (let* ((rows (length grid-lines))
         (cols (if (plusp rows) (length (car grid-lines)) 0))
         (grid-arr (make-array rows :initial-contents grid-lines))
         (visited (make-array (list rows cols) :initial-element nil))
         (total-price 0))
    
    (loop for r from 0 below rows do
      (loop for c from 0 below cols do
        (unless (aref visited r c)
          (multiple-value-bind (area perimeter)
              (calculate-region grid-arr rows cols r c visited)
            (incf total-price (* area perimeter))))))
    total-price))

(defun main ()
  (let* ((file-content
           (with-open-file (stream "input.txt" :direction :input)
             (loop for line = (read-line stream nil nil)
                   while line
                   when (string/= (string-trim '(#\Space #\Tab #\Newline #\Return) line) "")
                     collect line)))
         (total-price (solve file-content)))
    (format t "~a~%" total-price)))

(main)
