
(defun main ()
  (let* ((width 25)
         (height 6)
         (layer-size (* width height))
         (data (with-open-file (in "input.txt" :direction :input)
                 (read-line in nil nil))))
    (when (null data)
      (error "input.txt is empty or does not exist."))
    (setf data (string-trim '(#\Space #\Newline #\Return #\Tab) data))

    (let ((min-zeros (1+ layer-size))
          (result 0))
      (loop for i from 0 below (length data) by layer-size
            do (let* ((layer-end (min (+ i layer-size) (length data)))
                      (layer-str (subseq data i layer-end))
                      (zero-count 0)
                      (one-count 0)
                      (two-count 0))
                 (loop for char across layer-str
                       do (case char
                            (#\0 (incf zero-count))
                            (#\1 (incf one-count))
                            (#\2 (incf two-count))))
                 (when (< zero-count min-zeros)
                   (setf min-zeros zero-count)
                   (setf result (* one-count two-count)))))
      (format t "~a~%" result))))

(main)
