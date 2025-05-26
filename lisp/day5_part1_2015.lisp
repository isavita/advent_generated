
(defun nice-p (s)
  (let* ((vowels (loop for c across s count (find c "aeiou")))
         (double (loop for i from 0 below (1- (length s)) thereis (char= (char s i) (char s (1+ i)))))
         (forbidden (some #'(lambda (sub) (search sub s)) '("ab" "cd" "pq" "xy"))))
    (and (>= vowels 3) double (not forbidden))))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (format t "~a~%"
            (loop for l = (read-line f nil nil)
                  while l
                  count (nice-p l)))))

(main)
