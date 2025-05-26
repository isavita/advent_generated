
(defun main ()
  (let ((x (make-array 0 :element-type 'integer :fill-pointer 0 :adjustable t)))
    (vector-push-extend 1 x)

    (with-open-file (in "input.txt" :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((current-x (aref x (1- (length x)))))
                 (cond ((string= line "noop")
                        (vector-push-extend current-x x))
                       (t
                        (let ((n (parse-integer (subseq line 5))))
                          (vector-push-extend current-x x)
                          (vector-push-extend (+ current-x n) x)))))))

    (let ((grid (make-array '(6 40) :initial-element #\.)))
      (loop for i from 0 below (length x)
            for val = (aref x i)
            do (let* ((crtx (mod i 40))
                      (crty (floor i 40)))
                 (when (<= (abs (- crtx val)) 1)
                   (setf (aref grid crty crtx) #\#))))

      (loop for y from 0 below 6
            do (loop for x from 0 below 40
                     do (format t "~a" (aref grid y x)))
            do (format t "~%")))))

(main)
