
(defun is-trap (left center right)
  (cond
    ((and (char= left #\^) (char= center #\^) (char= right #\.)) #\^)
    ((and (char= center #\^) (char= right #\^) (char= left #\.)) #\^)
    ((and (char= left #\^) (char= center #\.) (char= right #\.)) #\^)
    ((and (char= right #\^) (char= center #\.) (char= left #\.)) #\^)
    (t #\.)))

(defun count-safe-tiles (row)
  (count #\. row))

(defun main ()
  (let* ((first-row (with-open-file (s "input.txt")
                      (string-trim '(#\Newline #\Return) (read-line s))))
         (current-row first-row)
         (safe-tiles (count-safe-tiles first-row))
         (num-generations 39))
    (loop for _ from 1 to num-generations
          do (let* ((row-length (length current-row))
                    (next-row-chars (make-string row-length)))
               (loop for i from 0 below row-length
                     for left = (if (= i 0) #\. (char current-row (1- i)))
                     for center = (char current-row i)
                     for right = (if (= i (1- row-length)) #\. (char current-row (1+ i)))
                     do (setf (char next-row-chars i) (is-trap left center right)))
               (setf current-row next-row-chars)
               (incf safe-tiles (count-safe-tiles current-row))))
    (print safe-tiles)))

(main)
