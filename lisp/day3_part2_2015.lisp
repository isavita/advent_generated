
(defun read-file-contents (f)
  (with-open-file (s f :direction :input)
    (let ((c (make-string (file-length s))))
      (read-sequence c s)
      c)))

(defun main ()
  (let* ((d (read-file-contents "input.txt"))
         (h (make-hash-table :test 'equal))
         (sx 0) (sy 0)
         (rx 0) (ry 0))
    (setf (gethash (list sx sy) h) t)
    (setf (gethash (list rx ry) h) t)
    (loop for i from 0 below (length d)
          for c = (char d i)
          do (if (evenp i)
                 (progn
                   (case c
                     (#\^ (incf sy))
                     (#\v (decf sy))
                     (#\> (incf sx))
                     (#\< (decf sx)))
                   (setf (gethash (list sx sy) h) t))
                 (progn
                   (case c
                     (#\^ (incf ry))
                     (#\v (decf ry))
                     (#\> (incf rx))
                     (#\< (decf rx)))
                   (setf (gethash (list rx ry) h) t))))
    (print (hash-table-count h))))

(main)
