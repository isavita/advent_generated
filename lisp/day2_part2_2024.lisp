
(defun split-string (string separator)
  (loop for i = 0 then (1+ j)
        as j = (position separator string :start i)
        for part = (subseq string i (or j (length string)))
        when (not (string= part ""))
          collect part
        while j))

(defun parse-input (file-path)
  (with-open-file (in file-path)
    (loop for line = (read-line in nil nil)
          while line
          collect (loop for s in (split-string line #\Space)
                        collect (parse-integer s)))))

(defun is-safe-sequence (report)
  (let ((len (length report)))
    (cond
      ((<= len 1) t)
      (t
       (let* ((x0 (first report))
              (x1 (second report))
              (diff0 (abs (- x0 x1)))
              (initial-direction (cond ((< x0 x1) :increasing)
                                       ((> x0 x1) :decreasing)
                                       (t :none))))

         (unless (and (<= 1 diff0) (<= diff0 3))
           (return-from is-safe-sequence nil))

         (when (eq initial-direction :none)
           (return-from is-safe-sequence nil))

         (loop for prev = x1 then curr
               for curr in (cddr report)
               for diff = (abs (- prev curr))
               do
                  (cond
                    ((< prev curr) (unless (eq initial-direction :increasing)
                                     (return-from is-safe-sequence nil)))
                    ((> prev curr) (unless (eq initial-direction :decreasing)
                                     (return-from is-safe-sequence nil)))
                    (t (return-from is-safe-sequence nil)))

                  (unless (and (<= 1 diff) (<= diff 3))
                    (return-from is-safe-sequence nil))))
       t))))

(defun is-safe (report)
  (when (is-safe-sequence report)
    (return-from is-safe t))

  (loop for i from 0 below (length report)
        for modified-report = (append (subseq report 0 i) (subseq report (1+ i)))
        do (when (is-safe-sequence modified-report)
             (return-from is-safe t)))
  nil)

(defun count-safe-reports (reports)
  (loop for report in reports
        count (is-safe report)))

(defun main ()
  (let ((reports (parse-input "input.txt")))
    (print (count-safe-reports reports))))

(main)
