
(defun main ()
  (let* ((input (with-open-file (f "input.txt") (read-line f)))
         (cups (map 'list #'digit-char-p input))
         (moves 100))
    (format t "~A~%" (solve cups moves))))

(defun solve (cups moves)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((max-cup (apply #'max cups))
         (cup-map (make-hash-table :test 'eql))
         (current-cup nil))

    (loop for i from 0 below (length cups) do
      (setf (gethash (nth i cups) cup-map)
            (the fixnum (nth (mod (the fixnum (1+ i)) (length cups)) cups))))

    (setf current-cup (the fixnum (first cups)))

    (loop for move from 1 to moves do
      (let* ((c1 (the fixnum (gethash current-cup cup-map)))
             (c2 (the fixnum (gethash c1 cup-map)))
             (c3 (the fixnum (gethash c2 cup-map)))
             (picked-up (list c1 c2 c3))
             (destination-cup (the fixnum (1- current-cup))))

        (setf (gethash current-cup cup-map) (the fixnum (gethash c3 cup-map)))

        (loop
          (when (and (> destination-cup 0) (not (member destination-cup picked-up)))
            (return))
          (setf destination-cup (the fixnum (if (<= destination-cup 0) max-cup (1- destination-cup)))))

        (setf (gethash c3 cup-map) (the fixnum (gethash destination-cup cup-map)))
        (setf (gethash destination-cup cup-map) c1)

        (setf current-cup (the fixnum (gethash current-cup cup-map)))))

    (let ((result-list '())
          (cup (the fixnum (gethash 1 cup-map))))
      (loop while (/= cup 1) do
        (push cup result-list)
        (setf cup (the fixnum (gethash cup cup-map))))
      (format nil "~{~A~}" (nreverse result-list)))))

(main)
