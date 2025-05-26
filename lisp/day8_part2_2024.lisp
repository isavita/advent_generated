(defun main ()
  (let* (grid h w antennas lines-per-freq antinodes)
    (with-open-file (f "input.txt" :direction :input)
      (setf grid (loop for line = (read-line f nil nil)
                       while line
                       collect (string-trim '(#\Newline #\Return) line))))
    (setf h (length grid))
    (setf w (length (first grid)))

    (setf antennas (make-hash-table :test 'eql))
    (loop for y from 0 below h
          for row = (nth y grid)
          do (loop for x from 0 below w
                   for char = (char row x)
                   when (char/= char #\.)
                   do (push (list y x) (gethash char antennas))))

    (setf lines-per-freq (make-hash-table :test 'eql))
    (maphash (lambda (freq coords)
               (let ((lines-set (make-hash-table :test 'equal)))
                 (loop for i from 0 below (length coords)
                       for A = (nth i coords)
                       do (loop for j from (1+ i) below (length coords)
                                for B = (nth j coords)
                                do (let* ((dy (- (first B) (first A)))
                                          (dx (- (second B) (second A)))
                                          (g (gcd dy dx))
                                          (sy (/ dy g))
                                          (sx (/ dx g)))
                                     (when (or (< sx 0) (and (= sx 0) (< sy 0)))
                                       (setf sx (- sx)
                                             sy (- sy)))
                                     (let ((c (- (* sy (second A)) (* sx (first A)))))
                                       (setf (gethash (list sx sy c) lines-set) t)))))
                 (setf (gethash freq lines-per-freq) lines-set)))
             antennas)

    (setf antinodes (make-hash-table :test 'equal))
    (maphash (lambda (freq lines-set)
               (declare (ignore freq))
               (maphash (lambda (line-key val)
                          (declare (ignore val))
                          (destructuring-bind (sx sy c) line-key
                            (cond
                              ((and (zerop sx) (zerop sy)) nil)
                              ((zerop sy)
                               (when (zerop (rem c sx))
                                 (let ((y (- (/ c sx))))
                                   (when (and (>= y 0) (< y h))
                                     (loop for x-coord from 0 below w
                                           do (setf (gethash (list y x-coord) antinodes) t))))))
                              ((zerop sx)
                               (when (zerop (rem c sy))
                                 (let ((x-coord (/ c sy)))
                                   (when (and (>= x-coord 0) (< x-coord w))
                                     (loop for y-coord from 0 below h
                                           do (setf (gethash (list y-coord x-coord) antinodes) t))))))
                              (t
                               (loop for y-coord from 0 below h
                                     do (let ((val (+ c (* sx y-coord))))
                                          (when (zerop (rem val sy))
                                            (let ((x-coord (/ val sy)))
                                              (when (and (>= x-coord 0) (< x-coord w))
                                                (setf (gethash (list y-coord x-coord) antinodes) t))))))))))
                        lines-set))
             lines-per-freq)

    (format t "~a~%" (hash-table-count antinodes))))

(main)