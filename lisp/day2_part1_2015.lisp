
(defun parse-dimensions (line)
  (let* ((pos1 (position #\x line))
         (l (parse-integer line :end pos1))
         (pos2 (position #\x line :start (1+ pos1)))
         (w (parse-integer line :start (1+ pos1) :end pos2))
         (h (parse-integer line :start (1+ pos2))))
    (values l w h)))

(defun main ()
  (let ((total-paper 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (multiple-value-bind (l w h) (parse-dimensions line)
                 (let* ((lw (* l w))
                        (wh (* w h))
                        (hl (* h l)))
                   (incf total-paper (+ (* 2 (+ lw wh hl))
                                        (min lw wh hl)))))))
    (format t "~a~%" total-paper)))

(main)
