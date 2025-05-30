
(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun parse-line (line)
  (let* ((eq-pos (position #\= line))
         (head (subseq line 0 (1- eq-pos)))
         (children-start-pos (+ eq-pos 3))
         (children-end-pos (position #\) line :from-end t))
         (children-str (subseq line children-start-pos children-end-pos))
         (comma-pos (position #\, children-str))
         (left (subseq children-str 0 comma-pos))
         (right (subseq children-str (+ comma-pos 2))))
    (values head left right)))

(defun parse-input (lines)
  (let ((instructions (first lines))
        (nodes (make-hash-table :test 'equal)))
    (loop for line in (cddr lines)
          do (multiple-value-bind (head left right) (parse-line line)
               (setf (gethash head nodes) (list left right))))
    (values instructions nodes)))

(defun lcm-slice (nums)
  (cond ((null nums) 0)
        (t (reduce #'lcm (cdr nums) :initial-value (car nums)))))

(defun solve (lines)
  (multiple-value-bind (instructions nodes) (parse-input lines)
    (let* ((start-nodes (loop for k being the hash-keys of nodes
                              when (char= #\A (char k (1- (length k))))
                              collect k))
           (steps-list '())
           (instructions-length (length instructions)))
      (loop for start-node in start-nodes
            do (let ((current-node start-node)
                     (step-count 0))
                 (loop until (char= #\Z (char current-node (1- (length current-node))))
                       do (let ((instruction (char instructions (mod step-count instructions-length))))
                            (setf current-node (if (char= instruction #\L)
                                                   (first (gethash current-node nodes))
                                                   (second (gethash current-node nodes))))
                            (incf step-count)))
                 (push step-count steps-list)))
      (lcm-slice (nreverse steps-list)))))

(defun main ()
  (let ((input-lines (read-file "input.txt")))
    (print (solve input-lines))))

(main)
