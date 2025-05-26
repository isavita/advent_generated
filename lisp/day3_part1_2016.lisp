
(defun main ()
  (let ((possible-triangles 0))
    (with-open-file (in "input.txt" :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((sides (with-input-from-string (s line)
                                (loop for num = (read s nil :eof)
                                      until (eq num :eof)
                                      collect num)))
                      (sorted-sides (sort sides #'<)))
                 (when (> (+ (nth 0 sorted-sides) (nth 1 sorted-sides)) (nth 2 sorted-sides))
                   (incf possible-triangles)))))
    (format t "~a~%" possible-triangles)))

(main)
