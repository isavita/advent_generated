
(defvar *data* nil)

(defun read-input ()
  (with-open-file (file "input.txt" :direction :input)
    (loop for num = (read file nil :eof)
          until (eq num :eof)
          collect num)))

(defun read-node ()
  (let* ((num-child-nodes (pop *data*))
         (num-metadata-entries (pop *data*))
         (total 0))
    (dotimes (i num-child-nodes)
      (incf total (read-node)))
    (dotimes (i num-metadata-entries)
      (incf total (pop *data*)))
    total))

(defun main ()
  (setf *data* (read-input))
  (format t "~A~%" (read-node)))

(main)
