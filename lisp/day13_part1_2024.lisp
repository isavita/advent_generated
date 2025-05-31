
(defun split-string (s delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter s :start i)
        collect (string-trim '(#\Space #\Tab) (subseq s i (or j (length s))))
        while j))

(defun string-starts-with-p (s prefix)
  (and (>= (length s) (length prefix))
       (string= s prefix :end1 (length prefix))))

(defun remove-prefix (s prefix)
  (if (string-starts-with-p s prefix)
      (subseq s (length prefix))
      s))

(defun parse-val (s)
  (let ((clean-s (string-trim '(#\Space #\Tab) s)))
    (loop for prefix in '("X+" "Y+" "X=" "Y=")
          do (setf clean-s (remove-prefix clean-s prefix)))
    (parse-integer clean-s)))

(defun parse-val-prize (s)
  (let ((clean-s (string-trim '(#\Space #\Tab) s)))
    (loop for prefix in '("X=" "Y=")
          do (setf clean-s (remove-prefix clean-s prefix)))
    (parse-integer clean-s)))

(defun parse-coords (s)
  (destructuring-bind (xp yp) (split-string s #\,)
    (list (parse-val xp) (parse-val yp))))

(defun parse-prize-coords (s)
  (destructuring-bind (xp yp) (split-string s #\,)
    (list (parse-val-prize xp) (parse-val-prize yp))))

(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun parse-machine (lines)
  (let (ax ay bx by px py)
    (loop for line in lines
          do (let* ((trimmed-line (string-trim '(#\Space #\Tab) line)))
               (cond ((string-starts-with-p trimmed-line "Button A:")
                      (destructuring-bind (x y) (parse-coords (subseq trimmed-line (length "Button A:")))
                        (setf ax x ay y)))
                     ((string-starts-with-p trimmed-line "Button B:")
                      (destructuring-bind (x y) (parse-coords (subseq trimmed-line (length "Button B:")))
                        (setf bx x by y)))
                     ((string-starts-with-p trimmed-line "Prize:")
                      (destructuring-bind (x y) (parse-prize-coords (subseq trimmed-line (length "Prize:")))
                        (setf px x py y))))))
    (list :ax ax :ay ay :bx bx :by by :px px :py py)))

(defun parse-input (lines)
  (let ((machines nil)
        (current-machine-lines nil))
    (loop for line in lines
          do (if (string= (string-trim '(#\Space #\Tab) line) "")
                 (progn
                   (when current-machine-lines
                     (push (parse-machine (nreverse current-machine-lines)) machines))
                   (setf current-machine-lines nil))
                 (push line current-machine-lines)))
    (when current-machine-lines
      (push (parse-machine (nreverse current-machine-lines)) machines))
    (nreverse machines)))

(defun solve-machine (m)
  (let ((min-cost -1)
        (max-presses 100))
    (loop for a-count from 0 to max-presses
          do (loop for b-count from 0 to max-presses
                   do (let* ((x (+ (* (getf m :ax) a-count) (* (getf m :bx) b-count)))
                             (y (+ (* (getf m :ay) a-count) (* (getf m :by) b-count))))
                        (when (and (= x (getf m :px)) (= y (getf m :py)))
                          (let ((cost (+ (* a-count 3) b-count)))
                            (if (or (< min-cost 0) (< cost min-cost))
                                (setf min-cost cost)))))))
    min-cost))

(defun main ()
  (let* ((input-lines (read-file-lines "input.txt"))
         (machines (parse-input input-lines))
         (results nil))
    (loop for m in machines
          do (let ((cost (solve-machine m)))
               (when (>= cost 0)
                 (push cost results))))
    (if (null results)
        (format t "0 0~%")
        (let* ((count (length results))
               (sum (loop for r in results sum r)))
          (format t "~A ~A~%" count sum)))))

(main)
