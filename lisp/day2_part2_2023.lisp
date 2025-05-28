
;; Define a custom string splitting function since Common Lisp doesn't have a built-in one like Python's .split()
(defun split-string-by-separator (string separator)
  "Splits a string by a given separator string.
   Returns a list of substrings, with leading/trailing spaces trimmed.
   This implementation handles multi-character separators correctly."
  (loop with start = 0
        with sep-len = (length separator)
        for end = (search separator string :start2 start)
        ;; Collect the substring from 'start' to 'end', trimming spaces.
        collect (string-trim '(#\Space) (subseq string start (or end (length string))))
        ;; If 'end' is NIL, it means no more separators were found, so we're done.
        unless end do (loop-finish)
        ;; Move 'start' past the found separator for the next iteration.
        do (setf start (+ end sep-len))))

(defun parse-game-line (line)
  "Parses a single game line, extracts its ID, and determines the maximum
   number of red, green, and blue cubes seen in any single reveal for that game.
   Returns three values: game-id, max-red, max-green, max-blue."
  (let* ((colon-pos (position #\: line))
         ;; Extract game ID: "Game N: ..." -> "N"
         ;; "Game " is 5 characters long, so we start at index 5.
         (game-id-str (subseq line 5 colon-pos))
         (game-id (parse-integer game-id-str))
         ;; Extract the game data: "...: R G B; R G B; ..."
         (data-str (subseq line (1+ colon-pos))) ; Skip the colon itself
         (max-red 0)
         (max-green 0)
         (max-blue 0))

    ;; Process each "hand" (subset of cubes revealed from the bag)
    (loop for hand-str in (split-string-by-separator data-str "; ")
          do (loop for cube-str in (split-string-by-separator hand-str ", ")
                   do (let* ((space-pos (position #\Space cube-str))
                             (count-str (subseq cube-str 0 space-pos))
                             (count (parse-integer count-str))
                             (color-str (subseq cube-str (1+ space-pos))))
                        ;; Update maximum counts for each color found in this game
                        (cond ((string= color-str "red")
                               (setf max-red (max max-red count)))
                              ((string= color-str "green")
                               (setf max-green (max max-green count)))
                              ((string= color-str "blue")
                               (setf max-blue (max max-blue count)))))))
    ;; Return the extracted values
    (values game-id max-red max-green max-blue)))

(defun main ()
  "Main entry point of the program for the Cube Conundrum challenge.
   Reads 'input.txt', processes each game line, and calculates
   the sum of possible game IDs (Part 1) and the sum of powers of minimum sets (Part 2).
   Outputs the results to standard output."
  (let ((sum-part1 0)
        (sum-part2 0))
    ;; Open the input file for reading
    (with-open-file (stream "input.txt" :direction :input
                                        :if-does-not-exist :error)
      ;; Loop through each line in the file until EOF
      (loop for line = (read-line stream nil nil) ; Read line, return NIL at EOF
            while line                            ; Continue as long as line is not NIL
            do (multiple-value-bind (game-id max-red max-green max-blue)
                   (parse-game-line line)
                 ;; Part 1: Check if the game is possible with the given bag limits.
                 ;; The Elf's bag contains: 12 red, 13 green, 14 blue cubes.
                 (when (and (<= max-red 12)
                            (<= max-green 13)
                            (<= max-blue 14))
                   (incf sum-part1 game-id))

                 ;; Part 2: Calculate the 'power' of the minimum set of cubes required.
                 ;; This is the product of the maximum observed counts for each color.
                 (incf sum-part2 (* max-red max-green max-blue)))))

    ;; Print the final results to standard output
    (format t "Part 1 Sum of possible game IDs: ~a~%" sum-part1)
    (format t "Part 2 Sum of powers: ~a~%" sum-part2)))

;; Execute the main function when this file is loaded or run.
;; For running from a REPL, simply call `(main)`.
;; For creating a standalone executable (e.g., with SBCL):
;; `(sb-ext:save-lisp-and-die "cube-conundrum" :toplevel #'main :executable t)`
(main)
