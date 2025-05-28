
;; Lisp program to solve Advent of Code Day 9: Mirage Maintenance - Part 1

;; Function to parse a line of space-separated integers into a list of integers.
;; Handles multiple spaces and leading/trailing spaces robustly.
(defun parse-numbers-from-string (s)
  (loop with result = '()
        with start = 0
        with len = (length s)
        for i from 0 to len
        when (or (= i len) (char= (char s i) #\Space))
        do (when (< start i) ; Only parse if there's a non-empty substring
             (push (parse-integer s :start start :end i) result))
           ;; Move start past the current space(s)
           (setf start (1+ i))
           ;; Skip any subsequent spaces (e.g., if input has "10  13")
           (loop while (and (< start len) (char= (char s start) #\Space))
                 do (incf start))
        finally (return (nreverse result)))) ; Efficiently reverse the list once

;; Function to compute the differences between consecutive elements in a list.
;; E.g., (A B C D) -> (B-A C-B D-C)
(defun get-differences (sequence)
  (loop for (a b) on sequence
        while b ; Ensure there's a next element to compute a difference with
        collect (- b a)))

;; Function to check if all elements in a list are zero.
(defun all-zeros-p (sequence)
  (every #'zerop sequence))

;; Helper to get the last element of a list.
;; (car (last list)) is the idiomatic Lisp way to achieve this.
(defun lastcar (list)
  (car (last list)))

;; Recursive function to extrapolate the next value in a history sequence.
;; This function directly mirrors the problem's described extrapolation process.
(defun extrapolate-next-value (history)
  (let ((diffs (get-differences history)))
    (if (all-zeros-p diffs)
        ;; Base case: if differences are all zeros, the next difference is 0.
        ;; So, the next value in the current history is just its last value.
        (lastcar history)
        ;; Recursive step: The next value in the current history is its last
        ;; value plus the extrapolated next value of its differences sequence.
        (+ (lastcar history) (extrapolate-next-value diffs)))))

;; Main entry point for the program.
;; Reads 'input.txt', processes each history line,
;; extrapolates its next value, sums them up,
;; and prints the total sum to standard output.
(defun main ()
  (let ((total-sum 0))
    (with-open-file (stream "input.txt"
                            :direction :input
                            :if-does-not-exist :error) ; Ensures the file exists
      (loop for line = (read-line stream nil nil) ; Read line by line until EOF
            while line
            do (let* ((history (parse-numbers-from-string line))
                      (next-value (extrapolate-next-value history)))
                 (incf total-sum next-value))))
    (format t "~a~%" total-sum))) ; Print the final sum followed by a newline

;; Call the main function when the script is loaded or run.
;; This is a standard way to execute a Lisp program.
(main)
