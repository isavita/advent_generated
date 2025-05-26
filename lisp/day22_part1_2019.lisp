
(defconstant +deck-size+ 10007)
(defconstant +target-card+ 2019)

(defun string-starts-with-p (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix (subseq string 0 (length prefix)))))

(defun string-split (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun main ()
  (let ((a 1)
        (b 0))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line do
            (cond
              ((string= line "deal into new stack")
               (setf a (mod (- a) +deck-size+))
               (setf b (mod (- (+ b 1)) +deck-size+)))
              ((string-starts-with-p "cut" line)
               (let* ((parts (string-split line #\Space))
                      (n (parse-integer (car (last parts)))))
                 (setf b (mod (- b n) +deck-size+))))
              ((string-starts-with-p "deal with increment" line)
               (let* ((parts (string-split line #\Space))
                      (n (parse-integer (car (last parts)))))
                 (setf a (mod (* a n) +deck-size+))
                 (setf b (mod (* b n) +deck-size+)))))))
    (format t "~a~%" (mod (+ (* a +target-card+) b) +deck-size+))))

(main)
