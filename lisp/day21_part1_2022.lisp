
(defun split-string-by-space (s)
  (loop with start = 0
        with len = (length s)
        for i from 0 to len
        when (or (= i len) (char= (char s i) #\Space))
        collect (subseq s start i)
        and do (setf start (1+ i))))

(defun calculate (monkey jobs results)
  (multiple-value-bind (value found) (gethash monkey results)
    (when found
      (return-from calculate value)))

  (let ((job (gethash monkey jobs)))
    (unless job
      (error "Monkey not found: ~a" monkey))

    (multiple-value-bind (num end-pos) (parse-integer job :junk-allowed t)
      (if (and num (= end-pos (length job)))
          (progn
            (setf (gethash monkey results) num)
            num)
          (let* ((parts (split-string-by-space job))
                 (a-monkey (nth 0 parts))
                 (op (nth 1 parts))
                 (b-monkey (nth 2 parts))
                 (val-a (calculate a-monkey jobs results))
                 (val-b (calculate b-monkey jobs results))
                 (result (cond
                           ((string= op "+") (+ val-a val-b))
                           ((string= op "-") (- val-a val-b))
                           ((string= op "*") (* val-a val-b))
                           ((string= op "/") (floor val-a val-b))
                           (t (error "Unknown operation: ~a" op)))))
            (setf (gethash monkey results) result)
            result)))))

(defun main ()
  (let ((jobs (make-hash-table :test 'equal))
        (results (make-hash-table :test 'equal)))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (let* ((colon-pos (position #\: line))
                      (monkey (subseq line 0 colon-pos))
                      (job-str (string-trim " " (subseq line (+ colon-pos 1)))) )
                 (setf (gethash monkey jobs) job-str))))
    (print (calculate "root" jobs results))))

(main)
