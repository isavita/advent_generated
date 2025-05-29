
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun parse-numbers-from-string (s)
  (mapcar #'parse-integer (remove-if (lambda (s) (string= s "")) (split-string s #\Space))))

(defun reverse-convert-number (number ranges)
  (dolist (r ranges number)
    (destructuring-bind (dest-start src-start length) r
      (when (and (>= number dest-start)
                 (< number (+ dest-start length)))
        (return-from reverse-convert-number (+ src-start (- number dest-start)))))))

(defun is-in-seed-ranges (number seed-ranges)
  (dolist (r seed-ranges nil)
    (destructuring-bind (start length) r
      (when (and (>= number start)
                 (< number (+ start length)))
        (return-from is-in-seed-ranges t)))))

(defun main ()
  (let* ((seed-ranges nil)
         (current-ranges nil)
         (maps nil))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (cond
                 ((search "map:" line)
                  (when current-ranges
                    (push (nreverse current-ranges) maps))
                  (setf current-ranges nil))
                 ((search "seeds:" line :start2 0)
                  (let ((parsed-numbers (parse-numbers-from-string (subseq line (length "seeds: ")))))
                    (setf seed-ranges
                          (loop for (s l) on parsed-numbers by #'cddr
                                collect (list s l)))))
                 (t
                  (let ((nums (parse-numbers-from-string line)))
                    (when (= (length nums) 3)
                      (push (list (first nums) (second nums) (third nums)) current-ranges)))))))
    (when current-ranges
      (push (nreverse current-ranges) maps))

    (setf maps (nreverse maps))

    (loop for location from 0
          do (let ((seed location))
               (dolist (map (reverse maps))
                 (setf seed (reverse-convert-number seed map)))
               (when (is-in-seed-ranges seed seed-ranges)
                 (print location)
                 (return))))))

(main)
