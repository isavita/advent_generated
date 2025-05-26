
(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (let* ((lines (loop for line = (read-line stream nil)
                        while line
                        collect line))
           (message-len (length (first lines)))
           (message1 (make-string message-len))
           (message2 (make-string message-len)))
      (loop for i from 0 below message-len do
        (let ((counts (make-hash-table :test 'eql))
              (first-char-set-p nil)
              (most-common-char nil)
              (most-common-count -1)
              (least-common-char nil)
              (least-common-count most-positive-fixnum))
          (loop for line in lines do
            (incf (gethash (char line i) counts 0)))
          (maphash (lambda (char count)
                     (if (not first-char-set-p)
                         (setf most-common-char char
                               most-common-count count
                               least-common-char char
                               least-common-count count
                               first-char-set-p t)
                         (progn
                           (when (or (> count most-common-count)
                                     (and (= count most-common-count)
                                          (< (char-code char) (char-code most-common-char))))
                             (setf most-common-char char
                                   most-common-count count))
                           (when (or (< count least-common-count)
                                     (and (= count least-common-count)
                                          (> (char-code char) (char-code least-common-char))))
                             (setf least-common-char char
                                   least-common-count count)))))
                   counts)
          (setf (char message1 i) most-common-char)
          (setf (char message2 i) least-common-char)))
      (format t "~A~%" message1)
      (format t "~A~%" message2))))

(main)
