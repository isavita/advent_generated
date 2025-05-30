
(defun parse-dimensions (line)
  (let* ((p1 (position #\x line))
         (l (parse-integer line :end p1))
         (p2 (position #\x line :start (1+ p1)))
         (w (parse-integer line :start (1+ p1) :end p2))
         (h (parse-integer line :start (1+ p2))))
    (values l w h)))

(defun main ()
  (let ((total-paper 0)
        (total-ribbon 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line do
            (multiple-value-bind (l w h) (parse-dimensions line)
              (let* ((lw (* l w))
                     (wh (* w h))
                     (hl (* h l)))
                (incf total-paper (+ (* 2 (+ lw wh hl)) (min lw wh hl)))
                (incf total-ribbon (+ (* 2 (min (+ l w) (+ w h) (+ h l))) (* l w h)))))))
    (format t "~a~%" total-paper)
    (format t "~a~%" total-ribbon)))

(main)
