
(defun calculate-priority (char)
  (let ((code (char-code char)))
    (if (>= code (char-code #\a))
        (- code (char-code #\a) -1)
        (- code (char-code #\A) -27))))

(defun make-priority-set (s)
  (let ((bv (make-array 53 :element-type 'bit :initial-element 0)))
    (loop for char across s do
      (setf (aref bv (calculate-priority char)) 1))
    bv))

(defun intersect-priority-sets (set1 set2)
  (bit-and set1 set2))

(defun part-one (lines)
  (loop for line in lines
        sum (let* ((mid (/ (length line) 2))
                   (first-compartment-set (make-priority-set (subseq line 0 mid)))
                   (second-compartment (subseq line mid)))
              (loop for char across second-compartment
                    when (= (aref first-compartment-set (calculate-priority char)) 1)
                      do (return (calculate-priority char))))))

(defun part-two (lines)
  (loop for i from 0 below (length lines) by 3
        sum (let* ((set1 (make-priority-set (nth i lines)))
                   (set2 (make-priority-set (nth (1+ i) lines)))
                   (set3 (make-priority-set (nth (+ i 2) lines)))
                   (common-set (intersect-priority-sets (intersect-priority-sets set1 set2) set3)))
              (loop for p from 1 to 52
                    when (= (aref common-set p) 1)
                      do (return p)))))

(defun main ()
  (let* ((lines (with-open-file (stream "input.txt" :direction :input)
                  (loop for line = (read-line stream nil nil)
                        while line
                        when (not (string= line ""))
                          collect line))))
    (format t "Part One: ~a~%" (part-one lines))
    (format t "Part Two: ~a~%" (part-two lines))))

(main)
