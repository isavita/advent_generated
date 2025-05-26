
(defun solve ()
  (let* ((grid nil)
         (h 0)
         (w 0)
         (antennas (make-hash-table))
         (antinodes (make-hash-table :test 'equal)))

    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (push line grid)))
    (setf grid (nreverse grid))
    (setf h (length grid))
    (when (> h 0)
      (setf w (length (elt grid 0))))

    (loop for y from 0 below h
          for row-str = (elt grid y)
          do (loop for x from 0 below w
                   for char = (char row-str x)
                   when (char/= char #\.)
                   do (push (list y x) (gethash char antennas nil))))

    (loop for coords being the hash-value of antennas
          do (loop for current-A-list on coords
                   for A = (car current-A-list)
                   do (loop for current-B-list on (cdr current-A-list)
                            for B = (car current-B-list)
                            do (let* ((ay (car A))
                                      (ax (cadr A))
                                      (by (car B))
                                      (bx (cadr B))
                                      (p1y (- (* 2 ay) by))
                                      (p1x (- (* 2 ax) bx))
                                      (p2y (- (* 2 by) ay))
                                      (p2x (- (* 2 bx) ax)))
                                 (when (and (>= p1y 0) (< p1y h)
                                            (>= p1x 0) (< p1x w))
                                   (setf (gethash (list p1y p1x) antinodes) t))
                                 (when (and (>= p2y 0) (< p2y h)
                                            (>= p2x 0) (< p2x w))
                                   (setf (gethash (list p2y p2x) antinodes) t))))))

    (format t "~a~%" (hash-table-count antinodes))))

(solve)
