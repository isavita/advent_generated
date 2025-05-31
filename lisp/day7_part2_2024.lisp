
(in-package :cl-user)

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun string-to-number (s)
  (if (string= s "")
      0
      (parse-integer s)))

(defun can-be-made-true (test-value numbers)
  (let* ((num-operators (1- (length numbers)))
         (operator-combinations (expt 3 num-operators)))
    (loop for i from 0 below operator-combinations
          do (let ((current-combination i)
                   (result (car numbers)))
               (loop for j from 0 below num-operators
                     for next-number-list on (cdr numbers)
                     for next-number = (car next-number-list)
                     do (let* ((power-of-3-divisor (expt 3 (- num-operators 1 j)))
                                (operator (mod (floor current-combination power-of-3-divisor) 3)))
                          (setf result
                                (case operator
                                  (0 (+ result next-number))
                                  (1 (* result next-number))
                                  (2 (parse-integer (format nil "~a~a" result next-number)))))))
               (when (= result test-value)
                 (return-from can-be-made-true t))))
    nil))

(defun main ()
  (let ((total-calibration-result 0))
    (with-open-file (in "input.txt" :direction :input :if-does-not-exist :error)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
                      (parts (split-string trimmed-line #\:))
                      (test-value (parse-integer (car parts)))
                      (numbers-str (string-trim '(#\Space) (cadr parts)))
                      (numbers (mapcar #'string-to-number (split-string numbers-str #\Space))))
                 (when (can-be-made-true test-value numbers)
                   (incf total-calibration-result test-value)))))
    (format t "~a~%" total-calibration-result)))

(main)
