
(defun main ()
  (let* ((memory-banks (with-open-file (s "input.txt")
                         (coerce (loop for num = (read s nil :eof)
                                       until (eq num :eof)
                                       collect num)
                                 'vector)))
         (num-banks (length memory-banks))
         (seen-states (make-hash-table :test 'equalp))
         (cycles 0)
         (first-seen-cycle 0))

    (loop
      (let* ((current-state-copy (copy-seq memory-banks)))
        (multiple-value-bind (value foundp) (gethash current-state-copy seen-states)
          (when foundp
            (setf first-seen-cycle value)
            (return)))
        (setf (gethash current-state-copy seen-states) cycles))

      (let* ((max-blocks (loop for x across memory-banks maximizing x))
             (max-idx (position max-blocks memory-banks :test #'=)))
        (setf (aref memory-banks max-idx) 0)
        (loop for i from 1 to max-blocks
              do (setf max-idx (mod (1+ max-idx) num-banks))
                 (incf (aref memory-banks max-idx))))
      (incf cycles))

    (format t "~A~%" cycles)
    (format t "~A~%" (- cycles first-seen-cycle))))

(main)
