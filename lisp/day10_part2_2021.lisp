
(defun get-closing-char (opening-char)
  (case opening-char
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (t #\Space)))

(defun check-and-complete-line (line)
  (let* ((pairings (make-hash-table))
         (score-values (make-hash-table))
         (stack '())
         (opening '(#\( #\[ #\{ #\<))
         (closing '(#\) #\] #\} #\>)))
    (setf (gethash #\) pairings) #\(
          (gethash #\] pairings) #\[
          (gethash #\} pairings) #\{
          (gethash #\> pairings) #\<)
    (setf (gethash #\) score-values) 1
          (gethash #\] score-values) 2
          (gethash #\} score-values) 3
          (gethash #\> score-values) 4)

    (loop for char across line
          do (cond
               ((member char opening) (push char stack))
               ((member char closing)
                (when (or (null stack)
                          (char/= (car stack) (gethash char pairings)))
                  (return-from check-and-complete-line (values 0 nil)))
                (pop stack))))

    (if (null stack)
        (values 0 nil)
        (let ((score 0))
          (loop for char in stack
                do (setf score (+ (* score 5) (gethash (get-closing-char char) score-values))))
          (values score t)))))

(defun main ()
  (let ((scores '()))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil :eof)
            until (eq line :eof)
            do (multiple-value-bind (score incomplete)
                   (check-and-complete-line line)
                 (when incomplete
                   (push score scores)))))
    (let* ((sorted-scores (sort scores #'<))
           (middle-score (nth (floor (length sorted-scores) 2) sorted-scores)))
      (format t "~a~%" middle-score))))

(main)
