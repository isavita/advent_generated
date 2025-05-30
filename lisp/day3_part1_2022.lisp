
(defun get-priority (item-char)
  (let ((code (char-code item-char)))
    (if (>= code (char-code #\a))
        (- code (char-code #\a) -1)
        (- code (char-code #\A) -27))))

(defun calculate-priority-sum (filename)
  (let ((total-priority 0))
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil :eof)
            until (eq line :eof)
            do (let* ((len (length line))
                      (mid (floor len 2))
                      (first-compartment-chars (make-array 128 :element-type 'boolean :initial-element nil)))
                 (loop for i from 0 below mid
                       do (setf (aref first-compartment-chars (char-code (char line i))) t))
                 (loop for i from mid below len
                       for item-char = (char line i)
                       when (aref first-compartment-chars (char-code item-char))
                         do (incf total-priority (get-priority item-char))
                            (return)))))
    total-priority))

(defun main ()
  (let ((priority-sum (calculate-priority-sum "input.txt")))
    (princ priority-sum)
    (terpri)))

(main)
