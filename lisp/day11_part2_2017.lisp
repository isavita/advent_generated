
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun main ()
  (let ((directions (with-open-file (f "input.txt")
                      (split-string (string-trim '(#\Newline #\Return) (read-line f)) #\,))))
    (let ((dx 0)
          (dy 0)
          (max-dist 0))
      (loop for direction in directions do
        (cond ((string= direction "n") (incf dy))
              ((string= direction "s") (decf dy))
              ((string= direction "ne") (incf dx))
              ((string= direction "sw") (decf dx))
              ((string= direction "nw") (decf dx) (incf dy))
              ((string= direction "se") (incf dx) (decf dy)))
        (setf max-dist (max max-dist (max (abs dx) (abs dy)))))
      (format t "~a~%~a~%" (max (abs dx) (abs dy)) max-dist))))

(main)
