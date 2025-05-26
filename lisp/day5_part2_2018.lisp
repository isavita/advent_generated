
(defun react (polymer-string)
  (let ((result '()))
    (loop for char across polymer-string do
      (if (and result
               (char/= char (car result))
               (char= (char-downcase char) (char-downcase (car result))))
          (pop result)
          (push char result)))
    (length result)))

(defun filter-polymer (polymer-string unit-char)
  (let ((upper-unit (char-upcase unit-char))
        (lower-unit (char-downcase unit-char)))
    (with-output-to-string (s)
      (loop for char across polymer-string do
        (unless (or (char= char upper-unit)
                    (char= char lower-unit))
          (write-char char s))))))

(defun get-unique-units (polymer-string)
  (remove-duplicates
    (loop for char across polymer-string
          collect (char-downcase char))
    :test 'char=))

(defun main ()
  (let* ((polymer-raw (with-open-file (f "input.txt" :direction :input)
                        (read-line f nil nil)))
         (initial-reacted-length (react polymer-raw)))
    (format t "~a~%" initial-reacted-length)

    (let ((min-length (length polymer-raw))
          (units (get-unique-units polymer-raw)))
      (loop for unit in units do
        (let* ((filtered-polymer (filter-polymer polymer-raw unit))
               (reacted-length (react filtered-polymer)))
          (when (< reacted-length min-length)
            (setf min-length reacted-length))))
      (format t "~a~%" min-length))))

(main)
