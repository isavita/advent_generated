
(defun split-string (s d)
  (loop for i = 0 then (1+ j)
        for j = (position d s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defun solve ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((l (read-line f nil ""))
           (p (sort (mapcar #'parse-integer (split-string l #\,)) #'<))
           (m (nth (floor (length p) 2) p)))
      (loop for x in p sum (abs (- x m))))))

(print (solve))
