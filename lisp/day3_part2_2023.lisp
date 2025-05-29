
(defun main ()
  (let ((grid (make-hash-table :test 'equal))
        (parts-list nil)
        (y 0))
    (with-open-file (in "input.txt" :direction :input)
      (loop for line = (read-line in nil nil)
            while line do
              (let ((curr-part nil))
                (loop for c across line
                      for x from 0 do
                        (setf (gethash (cons x y) grid) c)
                        (let ((digit (digit-char-p c)))
                          (if digit
                              (if curr-part
                                  (setf (cadddr curr-part) (+ (* (cadddr curr-part) 10) digit)
                                        (caddr curr-part) x)
                                  (setf curr-part (list y x x digit)))
                              (when curr-part
                                (push curr-part parts-list)
                                (setf curr-part nil)))))
                (when curr-part
                  (push curr-part parts-list)))
              (incf y)))

    (let* ((parts (make-array (length parts-list) :initial-contents (nreverse parts-list)))
           (parts-grid (make-hash-table :test 'equal))
           (sum 0))

      (loop for i from 0
            for part across parts do
              (let* ((part-y (first part))
                     (start-x (second part))
                     (end-x (third part)))
                (loop for x from start-x to end-x do
                  (setf (gethash (cons x part-y) parts-grid) i))))

      (maphash (lambda (coord char)
                 (when (char= char #\*)
                   (let* ((x (car coord))
                          (y (cdr coord))
                          (neighbor-coords (list (cons (- x 1) (- y 1))
                                                 (cons (- x 1) y)
                                                 (cons (- x 1) (+ y 1))
                                                 (cons x (- y 1))
                                                 (cons x (+ y 1))
                                                 (cons (+ x 1) (- y 1))
                                                 (cons (+ x 1) y)
                                                 (cons (+ x 1) (+ y 1))))
                          (distinct-part-indices (make-hash-table :test 'eql)))
                     (dolist (n-coord neighbor-coords)
                       (let ((part-idx (gethash n-coord parts-grid)))
                         (when part-idx
                           (setf (gethash part-idx distinct-part-indices) t))))
                     (when (= (hash-table-count distinct-part-indices) 2)
                       (let ((product 1))
                         (maphash (lambda (idx val)
                                    (declare (ignore val))
                                    (setf product (* product (cadddr (aref parts idx)))))
                                  distinct-part-indices)
                         (incf sum product))))))
               grid)
      (format t "~a~%" sum))))

(main)
