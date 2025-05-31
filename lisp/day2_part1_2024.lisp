
(defun parse-levels (line)
  (read-from-string (concatenate 'string "(" line ")")))

(defun is-safe-report (levels)
  (when (< (length levels) 2)
    (return-from is-safe-report nil))

  (let* ((first-val (first levels))
         (second-val (second levels))
         (first-diff (- second-val first-val)))

    (when (= first-diff 0)
      (return-from is-safe-report nil))

    (let ((is-increasing (> first-diff 0)))
      (loop for tail on levels
            for prev = (first tail)
            for current = (second tail)
            while current
            do (let ((diff (- current prev)))
                 (when (or (= diff 0)
                           (and is-increasing (<= diff 0))
                           (and (not is-increasing) (>= diff 0))
                           (> (abs diff) 3))
                   (return-from is-safe-report nil))))
      t)))

(defun main ()
  (handler-case
      (let ((safe-report-count 0))
        (with-open-file (stream "input.txt" :direction :input)
          (loop for line = (read-line stream nil)
                while line
                do (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                     (unless (string= trimmed-line "")
                       (let ((levels (parse-levels trimmed-line)))
                         (when (is-safe-report levels)
                           (incf safe-report-count)))))))
        (format t "~a~%" safe-report-count))
    (file-error (c)
      (format *error-output* "Error: ~a~%" c))))

(main)
