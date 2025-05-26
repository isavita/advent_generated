
(defun count-non-overlapping (substring string)
  (let ((count 0)
        (sub-len (length substring))
        (current-pos 0))
    (when (>= sub-len 1)
      (loop
        (let ((found-pos (search substring string :start2 current-pos)))
          (if found-pos
              (progn
                (incf count)
                (setf current-pos (+ found-pos sub-len)))
            (return)))))
    count))

(defun check-condition-1 (s)
  (let ((len (length s)))
    (when (>= len 2)
      (loop for i from 0 below (- len 1)
            for char-pair = (subseq s i (+ i 2))
            when (>= (count-non-overlapping char-pair s) 2)
              do (return t)))))

(defun check-condition-2 (s)
  (let ((len (length s)))
    (when (>= len 3)
      (loop for i from 0 below (- len 2)
            when (char= (char s i) (char s (+ i 2)))
              do (return t)))))

(defun main ()
  (let ((strings
          (with-open-file (stream "input.txt" :direction :input)
            (loop for line = (read-line stream nil nil)
                  while line
                  collect line))))
    (let ((nice-count 0))
      (loop for s in strings
            when (and (check-condition-1 s) (check-condition-2 s))
              do (incf nice-count))
      (format t "~a~%" nice-count))))

(main)
