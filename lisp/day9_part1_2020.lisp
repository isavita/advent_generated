
(defconstant +preamble-length+ 25)

(defun read-input (filepath)
  (with-open-file (stream filepath :direction :input)
    (coerce
     (loop for line = (read-line stream nil nil)
           while line
           for num = (parse-integer line :junk-allowed t)
           when num
           collect num)
     'vector)))

(defun find-first-invalid-number (numbers-vector)
  (loop for i from +preamble-length+ below (length numbers-vector)
        for current = (aref numbers-vector i)
        for preamble-start = (- i +preamble-length+)
        for preamble-end = i
        do
           (let* ((preamble-slice (subseq numbers-vector preamble-start preamble-end))
                  (previous-nums (make-hash-table :test 'eql)))
             (loop for x across preamble-slice
                   do (setf (gethash x previous-nums) t))

             (let ((is-valid nil))
               (loop for num across preamble-slice
                     do
                        (let ((complement (- current num)))
                          (when (and (gethash complement previous-nums)
                                     (/= num complement))
                            (setf is-valid t)
                            (return))))
               (unless is-valid
                 (return current))))))

(defun main ()
  (let* ((numbers (read-input "input.txt"))
         (first-invalid-number (find-first-invalid-number numbers)))
    (if first-invalid-number
        (format t "~a~%" first-invalid-number)
        (format t "All numbers are valid.~%"))))

(main)
