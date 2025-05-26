
(defparameter *keypad1*
  (make-array '(3 3) :initial-contents
              '((1 2 3)
                (4 5 6)
                (7 8 9))))

(defparameter *keypad2*
  (make-array '(5 5) :initial-contents
              '((0 0 1 0 0)
                (0 2 3 4 0)
                (5 6 7 8 9)
                (0 #\A #\B #\C 0)
                (0 0 #\D 0 0))))

(defun get-code (instructions keypad)
  (let* ((code (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (x 1)
         (y 1)
         (max-x (1- (array-dimension keypad 0)))
         (max-y (1- (array-dimension keypad 1))))
    (dolist (instruction instructions)
      (dolist (move (coerce instruction 'list))
        (let ((next-x x)
              (next-y y))
          (case move
            (#\U (when (> x 0) (decf next-x)))
            (#\D (when (< x max-x) (incf next-x)))
            (#\L (when (> y 0) (decf next-y)))
            (#\R (when (< y max-y) (incf next-y))))
          (unless (eql (aref keypad next-x next-y) 0)
            (setf x next-x
                  y next-y))))
      (vector-push-extend (char (princ-to-string (aref keypad x y)) 0) code))
    code))

(defun main ()
  (let ((instructions (with-open-file (in "input.txt")
                        (loop for line = (read-line in nil nil)
                              while line
                              collect line))))
    (princ (get-code instructions *keypad1*))
    (terpri)
    (princ (get-code instructions *keypad2*))
    (terpri)))

(main)
