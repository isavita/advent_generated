
(defun read-numbers-into-vector (filename)
  (let ((data (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream filename :direction :input)
      (loop for number = (read stream nil :eof)
            until (eq number :eof)
            do (vector-push-extend number data)))
    data))

(defun main ()
  (let* ((triangles (read-numbers-into-vector "input.txt"))
         (total-elements (length triangles))
         (num-cols 3)
         (num-rows (/ total-elements num-cols))
         (possible-triangles 0))

    (unless (= (mod total-elements num-cols) 0)
      (error "Input file contains a number of elements not divisible by 3."))
    (unless (= (mod num-rows 3) 0)
      (error "Input file contains a number of rows not divisible by 3."))

    (loop for i from 0 below num-rows by 3
          do (loop for j from 0 below num-cols
                   do (let* ((s1 (aref triangles (+ (* i num-cols) j)))
                             (s2 (aref triangles (+ (* (+ i 1) num-cols) j)))
                             (s3 (aref triangles (+ (* (+ i 2) num-cols) j)))
                             (sides (sort (list s1 s2 s3) #'<)))
                        (when (> (+ (first sides) (second sides)) (third sides))
                          (incf possible-triangles)))))

    (format t "~a~%" possible-triangles)))

(main)
