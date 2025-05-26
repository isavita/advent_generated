
(defconstant +keypad+
  (let ((kp (make-hash-table :test 'equal)))
    (setf (gethash '(0 0) kp) #\1)
    (setf (gethash '(1 0) kp) #\2)
    (setf (gethash '(2 0) kp) #\3)
    (setf (gethash '(0 1) kp) #\4)
    (setf (gethash '(1 1) kp) #\5)
    (setf (gethash '(2 1) kp) #\6)
    (setf (gethash '(0 2) kp) #\7)
    (setf (gethash '(1 2) kp) #\8)
    (setf (gethash '(2 2) kp) #\9)
    kp))

(defun main ()
  (let ((current-position (list 1 1))
        (result-stream (make-string-output-stream)))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line do
            (loop for char across line do
              (cond
                ((and (char= char #\U) (> (second current-position) 0))
                 (decf (second current-position)))
                ((and (char= char #\D) (< (second current-position) 2))
                 (incf (second current-position)))
                ((and (char= char #\L) (> (first current-position) 0))
                 (decf (first current-position)))
                ((and (char= char #\R) (< (first current-position) 2))
                 (incf (first current-position)))))
            (write-char (gethash current-position +keypad+) result-stream)))
    (princ (get-output-stream-string result-stream))))

(main)
