
(defun main ()
  (let* ((filepath "input.txt")
         (directions (with-open-file (s filepath :direction :input :if-does-not-exist :error)
                       (let ((str (make-string (file-length s))))
                         (read-sequence str s)
                         str))))
    (let ((houses (make-hash-table :test 'equal))
          (x 0)
          (y 0))
      (setf (gethash (list x y) houses) t)
      (loop for char across directions do
        (case char
          (#\^ (incf y))
          (#\v (decf y))
          (#\> (incf x))
          (#\< (decf x)))
        (setf (gethash (list x y) houses) t))
      (format t "~a~%" (hash-table-count houses)))))

(main)
