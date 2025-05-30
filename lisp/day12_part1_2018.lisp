
(defun get-char-or-default (hash-table key default-char)
  (multiple-value-bind (value found)
      (gethash key hash-table)
    (if found value default-char)))

(defun main ()
  (let (initial-state
        (rules (make-hash-table :test 'equal))
        (state (make-hash-table)))

    (with-open-file (in "input.txt" :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            do (cond ((search "initial state: " line)
                      (setf initial-state (subseq line (length "initial state: "))))
                     ((search " => " line)
                      (let* ((arrow-pos (search " => " line))
                             (pattern (subseq line 0 arrow-pos))
                             (result (subseq line (+ arrow-pos (length " => ")))))
                        (setf (gethash pattern rules) (char result 0)))))))

    (loop for i from 0 below (length initial-state)
          do (when (char= (char initial-state i) #\#)
               (setf (gethash i state) #\#)))

    (dotimes (generation 20)
      (let* ((min-pot (loop for k being the hash-keys of state minimize k))
             (max-pot (loop for k being the hash-keys of state maximize k))
             (new-state (make-hash-table)))

        (loop for i from (- min-pot 2) to (+ max-pot 2)
              do (let ((pattern (make-string 5)))
                   (loop for j from (- i 2) to (+ i 2)
                         for k from 0
                         do (setf (char pattern k) (get-char-or-default state j #\.)))
                   (when (char= (get-char-or-default rules pattern #\.) #\#)
                     (setf (gethash i new-state) #\#))))
        (setf state new-state)))

    (let ((sum 0))
      (loop for k being the hash-keys of state
            do (incf sum k))
      (print sum))))

(main)
