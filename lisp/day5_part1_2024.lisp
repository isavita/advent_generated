
(defun split-string (string delimiter)
  (loop with pos = 0
        with len = (length string)
        with del-len = (length delimiter)
        for next = (search delimiter string :start2 pos)
        collect (subseq string pos (or next len))
        do (if next
               (setq pos (+ next del-len))
               (loop-finish))))

(defun is-correct (update rules)
  (let ((pos (make-hash-table :test 'eql)))
    (loop for element in update
          for index from 0
          do (setf (gethash element pos) index))
    (dolist (rule rules t)
      (let ((x (first rule))
            (y (second rule)))
        (multiple-value-bind (x-idx x-found) (gethash x pos)
          (multiple-value-bind (y-idx y-found) (gethash y pos)
            (when (and x-found y-found)
              (when (> x-idx y-idx)
                (return nil)))))))))

(defun main ()
  (let ((rules (list))
        (updates (list))
        (total-sum 0))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            do (let ((stripped-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (unless (string= stripped-line "")
                   (if (find #\| stripped-line)
                       (let* ((parts (split-string stripped-line "|")))
                         (push (list (parse-integer (first parts))
                                     (parse-integer (second parts)))
                               rules))
                       (push (mapcar #'parse-integer (split-string stripped-line ","))
                             updates))))))
    (setq rules (nreverse rules))
    (setq updates (nreverse updates))
    (dolist (u updates)
      (when (is-correct u rules)
        (incf total-sum (nth (floor (/ (length u) 2)) u))))
    (format t "~a~%" total-sum)))

(main)
