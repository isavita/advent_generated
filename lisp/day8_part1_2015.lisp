
(defun calculate-difference (s)
  (let* ((len (length s))
         (diff 0)
         (i 0))
    ;; The outermost quotes '""' consume 2 code characters but 0 memory characters.
    ;; So, they contribute +2 to the total difference (code - memory).
    (incf diff 2)

    ;; Iterate through the string, handling escape sequences.
    ;; The loop processes characters from the beginning to the end,
    ;; with the initial `diff` increment handling the explicit outer quotes.
    (loop
      (when (>= i len) (return))
      (let ((char (char s i)))
        (cond
          ((char= char #\\)
           ;; Encountered a backslash, indicating an escape sequence.
           ;; Escape sequences consume more code characters than they represent in memory.
           (let ((next-char (char s (+ i 1))))
             (cond
               ((or (char= next-char #\\) (char= next-char #\"))
                ;; `\\` or `\"` escape sequence.
                ;; Consumes 2 code characters (`\` and `\` or `\` and `"`).
                ;; Represents 1 memory character (`\` or `"`).
                ;; Difference: 2 (code) - 1 (memory) = 1.
                (incf diff 1)
                (incf i 2))
               ((char= next-char #\x)
                ;; `\xHH` hexadecimal escape sequence.
                ;; Consumes 4 code characters (`\`, `x`, `H`, `H`).
                ;; Represents 1 memory character (the decoded character).
                ;; Difference: 4 (code) - 1 (memory) = 3.
                (incf diff 3)
                (incf i 4))
               (t
                ;; This case should not be reached with valid input for Advent of Code Day 8.
                ;; If it were, it implies a backslash followed by a non-special character,
                ;; consuming 2 code characters and representing 2 memory characters, so difference 0.
                (incf i 1)))))
          (t
           ;; A regular character.
           ;; Consumes 1 code character, represents 1 memory character.
           ;; Difference: 1 (code) - 1 (memory) = 0.
           (incf i 1)))))
    diff))

(defun main ()
  (let ((total-difference 0)
        ;; Construct the input file path relative to the script's location.
        (input-file (merge-pathnames "input.txt" (directory-namestring *load-pathname*))))
    (with-open-file (stream input-file
                            :direction :input
                            :if-does-not-exist :error)
      (loop for line = (read-line stream nil nil) ; Read line by line until EOF
            while line
            do (incf total-difference (calculate-difference line))))
    (princ total-difference)))

;; Call the main function to execute the program.
(main)
