
(defun split-string-by-char (string char)
  (loop for i = 0 then (1+ j)
        for j = (position char string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun find-nth-number (numbers turns)
  (let ((last-spoken (make-hash-table :test 'eql))
        (current-number 0))
    (loop for num in numbers
          for idx from 0
          do (setf (gethash num last-spoken) (1+ idx))
             (setf current-number num))

    (loop for turn from (1+ (length numbers)) to turns
          do (let ((last-occurrence (gethash current-number last-spoken)))
               (setf (gethash current-number last-spoken) (1- turn))
               (setf current-number
                     (if last-occurrence
                         (- (1- turn) last-occurrence)
                         0))))
    current-number))

(defun main ()
  (let* ((input-string (with-open-file (f "input.txt" :direction :input)
                         (read-line f)))
         (starting-numbers (mapcar #'parse-integer (split-string-by-char input-string #\,))))
    (print (find-nth-number starting-numbers 2020))))

(main)
