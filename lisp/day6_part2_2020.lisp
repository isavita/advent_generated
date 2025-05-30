(defun read-file-as-groups (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          with groups = '()
          with current-group-lines = '()
          while line
          do (if (string= line "")
                 (progn
                   (when current-group-lines
                     (push (reverse current-group-lines) groups))
                   (setf current-group-lines '()))
                 (push line current-group-lines))
          finally (progn
                    (when current-group-lines
                      (push (reverse current-group-lines) groups))
                    (return (reverse groups))))))

(defun get-unique-chars-in-group (group-lines)
  (length (remove-duplicates (loop for line in group-lines
                                   append (coerce line 'list)))))

(defun get-common-chars-in-group (group-lines)
  (when (null group-lines)
    (return-from get-common-chars-in-group 0))
  (let ((common-chars (remove-duplicates (coerce (first group-lines) 'list))))
    (loop for line in (rest group-lines)
          do (setf common-chars (intersection common-chars (remove-duplicates (coerce line 'list))))
          finally (return (length common-chars)))))

(defun count-yes-answers (groups all-flag)
  (loop for group in groups
        sum (if all-flag
                (get-common-chars-in-group group)
                (get-unique-chars-in-group group))))

(defun main ()
  (let* ((filename "input.txt")
         (groups (read-file-as-groups filename))
         (part-one-result (count-yes-answers groups nil))
         (part-two-result (count-yes-answers groups t)))
    (format t "Part One: ~a~%" part-one-result)
    (format t "Part Two: ~a~%" part-two-result)))

(main)