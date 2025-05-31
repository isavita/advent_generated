
(defun main ()
  (let ((total-count 0)
        (group-answers (make-hash-table :test 'eql)))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line do
            (if (string= line "")
                (progn
                  (incf total-count (hash-table-count group-answers))
                  (clrhash group-answers))
                (loop for char across line do
                      (setf (gethash char group-answers) t)))))
    (incf total-count (hash-table-count group-answers))
    (format t "~a~%" total-count)))

(main)
