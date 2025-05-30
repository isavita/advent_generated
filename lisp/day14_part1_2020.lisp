
(defun starts-with-p (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun apply-mask (value mask)
  (loop with result = value
        for i from 0 below 36
        for mask-char = (char mask (- 35 i))
        do (cond ((char= mask-char #\0)
                  (setf (ldb (byte 1 i) result) 0))
                 ((char= mask-char #\1)
                  (setf (ldb (byte 1 i) result) 1)))
        finally (return result)))

(defun main ()
  (let ((memory (make-hash-table :test 'eql))
        (current-mask nil))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (cond ((starts-with-p "mask = " line)
                      (setf current-mask (subseq line 7)))
                     ((starts-with-p "mem[" line)
                      (let* ((equal-pos (position #\= line))
                             (address-part (subseq line 0 (- equal-pos 1)))
                             (value-part (subseq line (+ equal-pos 2)))
                             (address (parse-integer (subseq address-part 4 (- (length address-part) 1))))
                             (value (parse-integer value-part)))
                        (setf (gethash address memory) (apply-mask value current-mask)))))))
    (let ((sum 0))
      (maphash (lambda (key val)
                 (declare (ignore key))
                 (incf sum val))
               memory)
      (format t "~a~%" sum))))

(main)
