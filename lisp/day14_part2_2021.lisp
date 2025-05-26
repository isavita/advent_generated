
(defun split-string (string delimiter)
  (loop with pos = 0
        with len = (length string)
        with delim-len = (length delimiter)
        for next-pos = (search delimiter string :start2 pos)
        collect (subseq string pos (or next-pos len))
        while next-pos
        do (setf pos (+ next-pos delim-len))
        when (>= pos len) do (loop-finish)))

(defun read-input (filename)
  (with-open-file (file filename :direction :input)
    (let* ((template (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line file nil nil)))
           (rules (make-hash-table :test 'equal)))
      (read-line file nil nil)
      (loop for line = (read-line file nil nil)
            while line
            do (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (parts (split-string trimmed-line " -> ")))
                 (when (= (length parts) 2)
                   (setf (gethash (nth 0 parts) rules) (nth 1 parts)))))
      (values template rules))))

(defun solve ()
  (multiple-value-bind (template rules) (read-input "input.txt")
    (let ((pair-counts (make-hash-table :test 'equal))
          (steps 40))
      (loop for i from 0 below (1- (length template))
            do (incf (gethash (subseq template i (+ i 2)) pair-counts 0)))

      (dotimes (step steps)
        (let ((new-pair-counts (make-hash-table :test 'equal)))
          (maphash (lambda (pair count)
                     (let ((insert (gethash pair rules)))
                       (if insert
                           (progn
                             (incf (gethash (concatenate 'string (subseq pair 0 1) insert) new-pair-counts 0) count)
                             (incf (gethash (concatenate 'string insert (subseq pair 1 2)) new-pair-counts 0) count))
                           (incf (gethash pair new-pair-counts 0) count))))
                   pair-counts)
          (setf pair-counts new-pair-counts)))

      (let ((element-counts (make-hash-table :test 'equal)))
        (maphash (lambda (pair count)
                   (incf (gethash (subseq pair 0 1) element-counts 0) count))
                 pair-counts)
        (incf (gethash (subseq template (1- (length template))) element-counts 0) 1)

        (let ((min-count most-positive-fixnum)
              (max-count 0))
          (maphash (lambda (element count)
                     (setf min-count (min min-count count))
                     (setf max-count (max max-count count)))
                   element-counts)
          (format t "~a~%" (- max-count min-count)))))))

(defun main ()
  (solve))

(main)
