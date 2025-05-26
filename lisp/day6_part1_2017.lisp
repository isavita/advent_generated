
(defun read-memory-banks (filename)
  (with-open-file (in filename :direction :input)
    (loop for x = (read in nil :eof)
          until (eq x :eof)
          collect x)))

(defun find-max-and-index (arr)
  (let ((max-val -1)
        (max-idx -1)
        (len (length arr)))
    (loop for i from 0 below len
          for val = (aref arr i)
          do (when (> val max-val)
               (setf max-val val
                     max-idx i)))
    (values max-val max-idx)))

(defun main ()
  (let* ((initial-banks-list (read-memory-banks "input.txt"))
         (memory-banks (make-array (length initial-banks-list) :initial-contents initial-banks-list))
         (seen-configs (make-hash-table :test 'equal))
         (cycles 0))
    (loop
      (let* ((current-config-list (coerce memory-banks 'list)))
        (multiple-value-bind (value found-p) (gethash current-config-list seen-configs)
          (declare (ignore value))
          (when found-p
            (format t "~a~%" cycles)
            (return)))
        (setf (gethash current-config-list seen-configs) cycles))

      (multiple-value-bind (max-blocks idx) (find-max-and-index memory-banks)
        (setf (aref memory-banks idx) 0)
        (loop for i from 1 to max-blocks
              for next-idx = (mod (+ idx i) (length memory-banks))
              do (incf (aref memory-banks next-idx))))
      (incf cycles))))

(main)
