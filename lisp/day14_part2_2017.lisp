
(defun knot-hash (input-string)
  (let* ((lengths (append (map 'list #'char-code (coerce input-string 'list))
                          '(17 31 73 47 23)))
         (list-arr (make-array 256 :initial-contents (loop for i from 0 below 256 collect i)))
         (position 0)
         (skip-size 0))

    (labels ((reverse-segment (arr start len total-len)
               (let ((end (+ start len -1)))
                 (loop for i from 0 below (floor len 2)
                       do (let* ((idx1 (mod (+ start i) total-len))
                                 (idx2 (mod (- end i) total-len))
                                 (val1 (aref arr idx1))
                                 (val2 (aref arr idx2)))
                            (setf (aref arr idx1) val2
                                  (aref arr idx2) val1)))))
             (perform-round (len-val)
               (reverse-segment list-arr position len-val 256)
               (setf position (mod (+ position len-val skip-size) 256))
               (incf skip-size)))

      (dotimes (round 64)
        (dolist (len lengths)
          (perform-round len))))

    (with-output-to-string (s)
      (dotimes (i 16)
        (let ((xor-result (aref list-arr (* i 16))))
          (loop for j from 1 below 16
                do (setf xor-result (logxor xor-result (aref list-arr (+ (* i 16) j)))))
          (format s "~2,'0X" xor-result))))))

(defun hex-to-binary (hex-string)
  (with-output-to-string (s)
    (loop for char across hex-string
          do (format s "~4,'0B" (parse-integer (string char) :radix 16)))))

(defun count-used-squares (grid)
  (loop for i from 0 below 128
        sum (loop for j from 0 below 128
                  sum (aref grid i j))))

(defun count-regions (grid)
  (let ((visited (make-array '(128 128) :initial-element nil)))
    (let ((region-count 0))
      (labels ((dfs (x y)
                 (when (and (>= x 0) (< x 128)
                            (>= y 0) (< y 128)
                            (= (aref grid x y) 1)
                            (not (aref visited x y)))
                   (setf (aref visited x y) t)
                   (dfs (+ x 1) y)
                   (dfs (- x 1) y)
                   (dfs x (+ y 1))
                   (dfs x (- y 1)))))
        (loop for i from 0 below 128 do
          (loop for j from 0 below 128 do
            (when (and (= (aref grid i j) 1)
                       (not (aref visited i j)))
              (incf region-count)
              (dfs i j)))))
      region-count)))

(defun main ()
  (let* ((input-string (with-open-file (s "input.txt" :direction :input)
                         (read-line s nil nil)))
         (grid (make-array '(128 128) :element-type 'bit)))

    (dotimes (i 128)
      (let* ((row-input (format nil "~a-~a" input-string i))
             (hash (knot-hash row-input))
             (binary-row (hex-to-binary hash)))
        (dotimes (j 128)
          (setf (aref grid i j) (if (char= (char binary-row j) #\1) 1 0)))))

    (let ((used-squares (count-used-squares grid))
          (regions (count-regions grid)))
      (format t "Used squares: ~a~%" used-squares)
      (format t "Regions: ~a~%" regions))))

(main)
