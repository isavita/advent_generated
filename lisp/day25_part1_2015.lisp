
(defun extract-numbers-from-string (s)
  (loop with numbers = '()
        with current-number = 0
        with in-number = nil
        for char across s
        do (if (digit-char-p char)
               (progn
                 (setf current-number (+ (* current-number 10) (digit-char-p char)))
                 (setf in-number t))
               (progn
                 (when in-number
                   (push current-number numbers)
                   (setf current-number 0)
                   (setf in-number nil))))
        finally (when in-number
                  (push current-number numbers))
                (return (nreverse numbers))))

(defun get-code-at-position (row col)
  (let* ((initial-code 20151125)
         (multiplier 252533)
         (modulus 33554393)
         (diagonal-sum (- (+ row col) 1))
         (total-position (+ (/ (* (- diagonal-sum 1) diagonal-sum) 2) col))
         (code initial-code))
    (loop for i from 2 to total-position
          do (setf code (rem (* code multiplier) modulus)))
    code))

(defun main ()
  (let ((input-file-path "input.txt"))
    (with-open-file (in input-file-path :direction :input :if-does-not-exist :error)
      (let* ((input-line (read-line in nil nil))
             (numbers (extract-numbers-from-string input-line))
             (row (first numbers))
             (col (second numbers)))
        (format t "~a~%" (get-code-at-position row col))))))

(main)
