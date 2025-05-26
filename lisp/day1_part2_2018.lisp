
(defun read-changes (filename)
  (with-open-file (f filename :direction :input)
    (loop for line = (read-line f nil nil)
          while line
          collect (parse-integer line))))

(defun main ()
  (let* ((changes (read-changes "input.txt"))
         (frequency 0)
         (seen (make-hash-table :test 'eql)))
    (setf (gethash frequency seen) t)
    (loop
      (dolist (change changes)
        (incf frequency change)
        (when (gethash frequency seen)
          (format t "~a~%" frequency)
          (return-from main nil))
        (setf (gethash frequency seen) t)))))

(main)
