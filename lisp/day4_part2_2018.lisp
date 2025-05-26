
(defvar *guards* (make-hash-table :test 'eql))
(defvar *current-guard* nil)
(defvar *falls-asleep* nil)

(defun parse-minute (log-line)
  (parse-integer (subseq log-line 15 17)))

(defun parse-guard-id (log-line)
  (let ((hash-pos (position #\# log-line)))
    (parse-integer (subseq log-line (1+ hash-pos) (position #\Space log-line :start (1+ hash-pos))))))

(defun main ()
  (let ((lines nil))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            do (push line lines)))
    (setf lines (sort lines #'string<))

    (loop for line in lines
          do
             (cond
               ((search "Guard" line)
                (setf *current-guard* (parse-guard-id line))
                (when (not (gethash *current-guard* *guards*))
                  (setf (gethash *current-guard* *guards*) (make-array 60 :initial-element 0))))
               ((search "falls asleep" line)
                (setf *falls-asleep* (parse-minute line)))
               ((search "wakes up" line)
                (let ((wakes-up (parse-minute line)))
                  (loop for i from *falls-asleep* below wakes-up
                        do (incf (aref (gethash *current-guard* *guards*) i)))))))

    (let ((sleepiest-guard nil)
          (max-total-sleep -1))
      (maphash (lambda (guard-id minutes-array)
                 (let ((total-sleep (loop for count across minutes-array sum count)))
                   (when (> total-sleep max-total-sleep)
                     (setf max-total-sleep total-sleep
                           sleepiest-guard guard-id))))
               *guards*)

      (let* ((sleepiest-guard-minutes (gethash sleepiest-guard *guards*))
             (most-common-minute-count -1)
             (most-common-minute nil))
        (loop for i from 0 below 60
              for count = (aref sleepiest-guard-minutes i)
              do (when (> count most-common-minute-count)
                   (setf most-common-minute-count count
                         most-common-minute i)))
        (format t "~a~%" (* sleepiest-guard most-common-minute))))

    (let ((most-frequent-minute-guard nil)
          (most-frequent-minute-idx nil)
          (most-frequent-minute-count -1))
      (maphash (lambda (guard-id minutes-array)
                 (loop for i from 0 below 60
                       for count = (aref minutes-array i)
                       do (when (> count most-frequent-minute-count)
                            (setf most-frequent-minute-count count
                                  most-frequent-minute-guard guard-id
                                  most-frequent-minute-idx i))))
               *guards*)
      (format t "~a~%" (* most-frequent-minute-guard most-frequent-minute-idx)))))

(main)
