
(defun main ()
  (let ((data
         (with-open-file (stream "input.txt"
                                 :direction :input
                                 :if-does-not-exist :error)
           (let* ((file-length (file-length stream))
                  (buffer (make-string file-length :element-type 'character)))
             (read-sequence buffer stream)
             (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed) buffer)))))
    (let ((score 0)
          (garbage-count 0)
          (in-garbage nil)
          (ignore-next nil))
      (loop for char across data
            do
            (cond
              (ignore-next
                (setf ignore-next nil))
              ((char= char #\!)
                (setf ignore-next t))
              (in-garbage
                (if (char= char #\>)
                    (setf in-garbage nil)
                    (incf garbage-count)))
              ((char= char #\<)
                (setf in-garbage t))
              ((char= char #\{)
                (incf score))
              ((char= char #\})
                nil)
              (t nil)))
      (format t "~a~%" score)
      (format t "~a~%" garbage-count))))

(main)
