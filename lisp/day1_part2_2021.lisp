
(defun read-depths (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect (parse-integer line))))

(defun solve-part1 (depths)
  (loop for prev = (car depths) then current
        for current in (cdr depths)
        count (> current prev)))

(defun solve-part2 (depths)
  (loop for d1 in depths
        for d4 in (nthcdr 3 depths)
        while d4
        count (> d4 d1)))

(defun main ()
  (let* ((depths (read-depths "input.txt"))
         (part1-result (solve-part1 depths))
         (part2-result (solve-part2 depths)))
    (format t "~a~%" part1-result)
    (format t "~a~%" part2-result)))

(main)
