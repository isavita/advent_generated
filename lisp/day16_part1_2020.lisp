
(defun read-file-into-string (filename)
  (with-open-file (s filename :direction :input :if-does-not-exist :error)
    (let* ((len (file-length s))
           (data (make-string len)))
      (read-sequence data s)
      data)))

(defun split-by-string (string delimiter)
  (loop with results = '()
        with start = 0
        with delim-len = (length delimiter)
        for end = (search delimiter string :start2 start)
        do (push (subseq string start (or end (length string))) results)
        when (null end) do (return (nreverse results))
        do (setf start (+ end delim-len))))

(defstruct rule
  name
  (ranges nil :type list))

(defun parse-input-string (input-string)
  (let* ((sections (split-by-string input-string (format nil "~%~%")))
         (rules-section (first sections))
         (my-ticket-section (second sections))
         (nearby-tickets-section (third sections)))

    (let ((rules (loop for line in (split-by-string rules-section (format nil "~%"))
                       collect (destructuring-bind (name-part ranges-part)
                                   (split-by-string line ": ")
                                 (let ((name (string-trim " " name-part))
                                       (ranges (loop for range-str in (split-by-string ranges-part " or ")
                                                     collect (destructuring-bind (min-str max-str)
                                                                 (split-by-string range-str "-")
                                                               (list (parse-integer min-str) (parse-integer max-str))))))
                                   (make-rule :name name :ranges ranges)))))

          (my-ticket (mapcar #'parse-integer
                             (split-by-string (second (split-by-string my-ticket-section (format nil "~%"))) ",")))

          (nearby-tickets (loop for line in (rest (split-by-string nearby-tickets-section (format nil "~%")))
                                collect (mapcar #'parse-integer (split-by-string line ",")))))
      (values rules my-ticket nearby-tickets))))

(defun is-valid-value (value rules)
  (some (lambda (rule)
          (some (lambda (range)
                  (destructuring-bind (min max) range
                    (and (>= value min) (<= value max))))
                (rule-ranges rule)))
        rules))

(defun calculate-error-rate (tickets rules)
  (loop for ticket in tickets
        sum (loop for value in ticket
                  when (not (is-valid-value value rules))
                  sum value)))

(defun main ()
  (let ((input-string (read-file-into-string "input.txt")))
    (multiple-value-bind (rules my-ticket nearby-tickets)
        (parse-input-string (string-trim '(#\Newline #\Return #\Space #\Tab) input-string))
      (declare (ignore my-ticket))
      (let ((error-rate (calculate-error-rate nearby-tickets rules)))
        (format t "~a~%" error-rate)))))

(main)
