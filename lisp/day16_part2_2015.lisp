
(defun split-string (string &optional (separator #\Space))
  (loop for i = 0 then (1+ j)
        for j = (position separator string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun create-mfcsam-table ()
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key val) in '(("children" 3) ("cats" 7) ("samoyeds" 2) ("pomeranians" 3)
                             ("akitas" 0) ("vizslas" 0) ("goldfish" 5) ("trees" 3)
                             ("cars" 2) ("perfumes" 1))
          do (setf (gethash key ht) val))
    ht))

(defun main ()
  (let ((mfcsam (create-mfcsam-table)))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            do (let* ((parts (split-string line))
                      (sue-num (parse-integer (subseq (elt parts 1) 0 (1- (length (elt parts 1))))))
                      (valid t))
                 (loop for i from 2 below (length parts) by 2
                       do (let* ((prop-str (elt parts i))
                                 (val-str (elt parts (1+ i)))
                                 (prop (subseq prop-str 0 (1- (length prop-str))))
                                 (val (parse-integer (string-trim '(#\,) val-str))))
                            (cond
                              ((member prop '("cats" "trees") :test 'string=)
                               (when (>= (gethash prop mfcsam) val)
                                 (setf valid nil)
                                 (return)))
                              ((member prop '("pomeranians" "goldfish") :test 'string=)
                               (when (<= (gethash prop mfcsam) val)
                                 (setf valid nil)
                                 (return)))
                              (t
                               (when (/= (gethash prop mfcsam) val)
                                 (setf valid nil)
                                 (return))))))
                 (when valid
                   (format t "~a~%" sue-num)))))))

(main)
