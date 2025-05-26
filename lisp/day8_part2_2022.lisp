
(defun main ()
  (let ((grid (make-hash-table :test 'equal))
        (y 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line do
              (loop for char across line
                    for x from 0 do
                      (setf (gethash (list x y) grid) (digit-char-p char)))
              (incf y)))

    (let ((neighbors4 '((0 1) (0 -1) (1 0) (-1 0)))
          (max-score 0))
      (loop for p being the hash-key of grid do
        (let* ((current-height (gethash p grid))
               (score 1))
          (loop for n in neighbors4 do
            (let ((next-pos p)
                  (view 0))
              (loop
                (let* ((nx (+ (first next-pos) (first n)))
                       (ny (+ (second next-pos) (second n)))
                       (new-pos (list nx ny)))
                  (setf next-pos new-pos)

                  (let ((neighbor-height (gethash new-pos grid)))
                    (when neighbor-height
                      (incf view)
                      (when (>= neighbor-height current-height)
                        (setf score (* score view))
                        (return)))
                    (unless neighbor-height
                      (setf score (* score view))
                      (return)))))))
          (setf max-score (max max-score score))))
      (print max-score))))

(main)
