
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun egcd (a b)
  (cond ((zerop a) (list b 0 1))
        (t (let ((res (egcd (mod b a) a)))
             (list (first res)
                   (- (third res) (* (floor b a) (second res)))
                   (second res))))))

(defun mod-inverse (a m)
  (let ((res (egcd a m)))
    (mod (second res) m)))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (read-line f)
    (let* ((line (read-line f))
           (buses (loop for s in (split-string line #\,)
                        for i from 0
                        when (string/= s "x")
                          collect (list (parse-integer s) i))))
      (let ((t-val 0)
            (step 1))
        (loop for (bus offset) in buses
              do (let* ((remainder-needed (mod (- offset) bus))
                        (current-t-remainder (mod t-val bus))
                        (diff (mod (- remainder-needed current-t-remainder) bus))
                        (k (mod (* diff (mod-inverse step bus)) bus)))
                   (incf t-val (* k step)))
                 (setf step (* step bus)))
        (print t-val)))))

(main)
