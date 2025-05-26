
(defun main ()
  (let* ((jumps (with-open-file (f "input.txt" :direction :input)
                  (loop for line = (read-line f nil nil)
                        while line
                        collect (parse-integer line))))
         (jump-array (make-array (length jumps) :element-type 'fixnum :initial-contents jumps))
         (index 0)
         (steps 0))
    (loop
      (when (or (< index 0) (>= index (length jump-array)))
        (return))
      (let* ((offset (aref jump-array index)))
        (setf (aref jump-array index)
              (if (< offset 3)
                  (1+ offset)
                  (1- offset)))
        (incf index offset)
        (incf steps)))
    (print steps)))

(main)
