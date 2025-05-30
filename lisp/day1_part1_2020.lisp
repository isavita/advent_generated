
(defun find-product-of-entries (target file-path)
  (let ((expenses (make-hash-table)))
    (with-open-file (stream file-path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((entry (parse-integer line))
                      (complement (- target entry)))
                 (when (gethash complement expenses)
                   (return-from find-product-of-entries (* entry complement)))
                 (setf (gethash entry expenses) t))))
    (error "No two entries sum to the target.")))

(defun main ()
  (handler-case
      (let ((result (find-product-of-entries 2020 "input.txt")))
        (format t "~a~%" result))
    (error (c)
      (format *error-output* "~a~%" c))))

(main)
