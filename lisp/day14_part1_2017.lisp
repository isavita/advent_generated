
(defun knot-hash (input-string)
  (let* ((lengths (append (map 'list #'char-code (coerce input-string 'list))
                          '(17 31 73 47 23)))
         (list-data (make-array 256 :initial-contents (loop for i from 0 below 256 collect i)))
         (position 0)
         (skip-size 0))
    (loop for round from 0 below 64 do
      (loop for length in lengths do
        (unless (zerop length)
          (let ((sublist-temp (make-array length)))
            (loop for i from 0 below length do
              (setf (aref sublist-temp i) (aref list-data (mod (+ position i) 256))))
            (loop for i from 0 below length do
              (setf (aref list-data (mod (+ position i) 256))
                    (aref sublist-temp (- length 1 i))))))
        (setf position (mod (+ position length skip-size) 256))
        (incf skip-size)))

    (let ((dense-hash (make-list 16)))
      (loop for i from 0 below 16 do
        (setf (nth i dense-hash)
              (reduce #'logxor (subseq list-data (* i 16) (* (1+ i) 16)))))
      (format nil "~{~2,'0X~}" dense-hash))))

(defparameter *hex-to-binary-map*
  (let ((map (make-hash-table :test 'eql)))
    (loop for i from 0 to 15 do
      (setf (gethash (char (format nil "~X" i) 0) map)
            (format nil "~4,'0B" i)))
    map))

(defun hex-to-binary (hex-string)
  (with-output-to-string (s)
    (loop for char across hex-string do
      (write-string (gethash char *hex-to-binary-map*) s))))

(defun main ()
  (let ((input-key (with-open-file (in "input.txt" :direction :input)
                     (read-line in nil nil)))
        (used-squares-count 0))
    (loop for row from 0 below 128 do
      (let* ((hash-input (format nil "~a-~a" input-key row))
             (hash-value (knot-hash hash-input))
             (binary-representation (hex-to-binary hash-value)))
        (incf used-squares-count (count #\1 binary-representation))))
    (format t "~a~%" used-squares-count)))

(main)
