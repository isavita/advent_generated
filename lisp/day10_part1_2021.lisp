
(defconstant +pairings+
  (let ((h (make-hash-table :test 'eql)))
    (setf (gethash #\) h) #\(
          (gethash #\] h) #\[
          (gethash #\} h) #\{
          (gethash #\> h) #\<)
    h))

(defconstant +scores+
  (let ((h (make-hash-table :test 'eql)))
    (setf (gethash #\) h) 3
          (gethash #\] h) 57
          (gethash #\} h) 1197
          (gethash #\> h) 25137)
    h))

(defun check-line (line)
  (let ((stack '()))
    (loop for char across line
          do (case char
               ((#\( #\[ #\{ #\<)
                (push char stack))
               ((#\) #\] #\} #\>)
                (if (or (null stack)
                        (not (eql (car stack) (gethash char +pairings+))))
                    (return-from check-line (list (gethash char +scores+) t)))
                (pop stack))))
    (list 0 nil)))

(defun main ()
  (let ((total-score 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (let ((result (check-line line)))
                 (when (second result)
                   (incf total-score (first result))))))
    (format t "~A~%" total-score)))

(main)
