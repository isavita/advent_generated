
(defun main ()
  (let* ((jumps
           (with-open-file (stream "input.txt" :direction :input)
             (let ((lines (loop for line = (read-line stream nil :eof)
                                while (not (eq line :eof))
                                collect (parse-integer line))))
               (make-array (length lines)
                           :initial-contents lines
                           :element-type 'fixnum))))
         (len (length jumps))
         (current-index 0)
         (steps 0))
    (loop
      (when (or (< current-index 0) (>= current-index len))
        (return))
      (let* ((offset (aref jumps current-index))
             (next-index (+ current-index offset)))
        (incf (aref jumps current-index))
        (setf current-index next-index)
        (incf steps)))
    (format t "~a~%" steps)))

(main)
