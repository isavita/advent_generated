
(defun main ()
  (let* ((lines (coerce (with-open-file (s "input.txt")
                           (loop for l = (read-line s nil nil)
                                 while l
                                 collect l))
                        'vector))
         (h (length lines))
         (w (if (> h 0) (length (aref lines 0)) 0)))

    (loop with tc = 0
          with x = 0
          with y = 0
          while (< y h)
          do (when (char= #\# (char (aref lines y) (mod x w)))
               (incf tc))
             (incf x 3)
             (incf y)
          finally (format t "~a~%" tc))))

(main)
