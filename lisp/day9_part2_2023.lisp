
;;; Day 9: Mirage Maintenance

;; Utility to parse a line of space-separated numbers into a list of integers.
;; Example: "0 3 6 9 12 15" -> (0 3 6 9 12 15)
(defun parse-numbers-from-string (s)
  (with-input-from-string (in s)
    (loop for num = (read in nil :eof) ; read returns :eof at end of stream
          until (eq num :eof)
          collect num)))

;; Checks if all elements in a list are zero.
(defun all-zeros-p (sequence)
  (every #'zerop sequence))

;; Generates a list of difference sequences for a given history.
;; Returns a list where the first element is the original history,
;; and the last is the all-zero sequence.
(defun generate-difference-sequences (history)
  (let ((sequences (list history))) ; Start with the original history
    (loop
      (let* ((current-seq (first sequences))
             ;; Compute differences: (s2-s1 s3-s2 ...)
             (diff-seq (loop for (a b) on current-seq
                             while b ; Ensure we have at least two elements (a and b)
                             collect (- b a))))
        (push diff-seq sequences) ; Add the new difference sequence to the front
        (when (all-zeros-p diff-seq) ; Stop if the difference sequence is all zeros
          (return))))
    (nreverse sequences))) ; Reverse the list to get them in ascending order of depth

;; Extrapolates the next value for a given list of difference sequences.
;; Assumes sequences are in order from original history to all-zeros.
(defun extrapolate-next-value (sequences)
  (let ((extrapolated-val-below 0)) ; The extrapolated value for the sequence directly below (initially 0 for the all-zeros sequence)
    ;; Iterate from the second-to-last sequence up to the original history.
    ;; The last sequence is all zeros, its "next" value is implicitly 0.
    (loop for i from (- (length sequences) 2) downto 0
          do (let* ((current-seq (nth i sequences))
                    (last-val-in-current-seq (car (last current-seq))))
               ;; The next value for the current sequence is its last value
               ;; plus the extrapolated value from the sequence below it.
               (setf extrapolated-val-below (+ last-val-in-current-seq extrapolated-val-below))))
    extrapolated-val-below)) ; The final value is the one extrapolated for the original history

;; Extrapolates the previous value for a given list of difference sequences.
;; Assumes sequences are in order from original history to all-zeros.
(defun extrapolate-prev-value (sequences)
  (let ((extrapolated-val-below 0)) ; The extrapolated value for the sequence directly below (initially 0 for the all-zeros sequence)
    ;; Iterate from the second-to-last sequence up to the original history.
    ;; The last sequence is all zeros, its "previous" value is implicitly 0.
    (loop for i from (- (length sequences) 2) downto 0
          do (let* ((current-seq (nth i sequences))
                    (first-val-in-current-seq (car current-seq)))
               ;; The previous value for the current sequence is its first value
               ;; minus the extrapolated value from the sequence below it.
               (setf extrapolated-val-below (- first-val-in-current-seq extrapolated-val-below))))
    extrapolated-val-below)) ; The final value is the one extrapolated for the original history

;; Main function to solve the puzzle.
;; This function reads input, processes it, and prints the results.
(defun main ()
  (let ((total-sum-part1 0)
        (total-sum-part2 0))
    ;; Open input.txt for reading
    (with-open-file (stream "input.txt" :direction :input)
      ;; Loop through each line of the file until EOF
      (loop for line = (read-line stream nil :eof)
            until (eq line :eof)
            do (let* ((history (parse-numbers-from-string line))
                      (diff-sequences (generate-difference-sequences history)))
                 ;; Calculate and accumulate sums for both parts
                 (incf total-sum-part1 (extrapolate-next-value diff-sequences))
                 (incf total-sum-part2 (extrapolate-prev-value diff-sequences)))))

    ;; Print the final results
    (format t "Part 1 Total Sum: ~a~%" total-sum-part1)
    (format t "Part 2 Total Sum: ~a~%" total-sum-part2)))

;; Call the main function when the script is loaded or executed.
;; This is the standard entry point for a simple Lisp script.
(main)
