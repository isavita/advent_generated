
(defun read-numbers-from-file (filename)
  (coerce (with-open-file (stream filename :direction :input)
            (loop for line = (read-line stream nil nil)
                  while line
                  collect (parse-integer line)))
          'vector))

(defun main ()
  (let* ((numbers (read-numbers-from-file "input.txt"))
         (invalid-num 14360655)
         (len (length numbers)))
    (loop for i from 0 below len
          do (let ((total (aref numbers i))
                   (j (+ i 1)))
               (loop while (and (< total invalid-num)
                                (< j len))
                     do (incf total (aref numbers j))
                        (incf j))
               (when (= total invalid-num)
                 (let* ((contiguous-set-vector (subseq numbers i j))
                        (contiguous-set-list (coerce contiguous-set-vector 'list)))
                   (let ((weakness (+ (apply #'min contiguous-set-list)
                                      (apply #'max contiguous-set-list))))
                     (format t "~a~%" weakness)
                     (return-from main))))))))

(main)
