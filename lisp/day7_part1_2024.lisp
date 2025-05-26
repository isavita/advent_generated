
(in-package :cl)

(defun split-string (s delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun parse-input (filename)
  (with-open-file (file filename :direction :input)
    (loop for line = (read-line file nil nil)
          while line
          collect (let* ((colon-pos (position #\: line))
                         (test-value (parse-integer (subseq line 0 colon-pos)))
                         (numbers-str (subseq line (+ colon-pos 2)))
                         (numbers (mapcar #'parse-integer (split-string numbers-str #\Space))))
                    (list test-value numbers)))))

(defun generate-op-combinations (num-operators operators)
  (cond ((zerop num-operators) '(()))
        (t (loop for op in operators
                 nconc (loop for rest-comb in (generate-op-combinations (1- num-operators) operators)
                             collect (cons op rest-comb))))))

(defun evaluate-expression (test-value numbers)
  (let* ((num-operators (- (length numbers) 1)))
    (if (zerop num-operators)
        (= (car numbers) test-value)
        (let ((operators '(+ *)))
          (loop for ops-comb in (generate-op-combinations num-operators operators)
                do (let ((result (car numbers)))
                     (loop for op in ops-comb
                           for num in (cdr numbers)
                           do (setf result (funcall op result num)))
                     (when (= result test-value)
                       (return-from evaluate-expression t)))))))
  nil)

(defun calculate-sum-of-test-values (parsed-data)
  (loop for (test-val nums) in parsed-data
        summing (if (evaluate-expression test-val nums)
                    test-val
                    0)))

(defun main ()
  (let* ((input-data (parse-input "input.txt"))
         (total-sum (calculate-sum-of-test-values input-data)))
    (format t "~a~%" total-sum)))

(main)
