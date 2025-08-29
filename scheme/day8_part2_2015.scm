
; Filename: day8.scm

; --- Part 1: Calculate code_length - memory_length ---
; This function calculates the "in-memory" length of a string literal.
; It parses the string, handling escape sequences:
; - \\ (two backslashes) represents a single backslash.
; - \" (backslash followed by double-quote) represents a single double-quote.
; - \xHH (backslash, 'x', and two hexadecimal digits) represents a single character.
; The input string `s` is assumed to be a valid double-quoted string literal.
(define (calculate-memory-length s)
  (let ((len (string-length s)))
    ; The loop iterates from index 1 to len-2 (inclusive), effectively skipping the outer quotes.
    ; For an empty string literal `""` (length 2), the loop condition `(>= idx (- len 1))`
    ; becomes `(>= 1 1)`, which is true, and `mem-len` remains 0, which is correct.
    (let loop ((idx 1) (mem-len 0))
      (if (>= idx (- len 1)) ; Check if we've reached or passed the closing quote
          mem-len
          (let ((char (string-ref s idx)))
            (cond
              ((char=? char #\\) ; Encountered a backslash, check for escape sequence
               (let ((next-char (string-ref s (+ idx 1))))
                 (cond
                   ((or (char=? next-char #\\) (char=? next-char #\"))
                    ; \\ or \" escape sequence: consumes 2 characters from code, adds 1 to memory.
                    (loop (+ idx 2) (+ mem-len 1)))
                   ((char=? next-char #\x)
                    ; \xHH hexadecimal escape: consumes 4 characters from code, adds 1 to memory.
                    (loop (+ idx 4) (+ mem-len 1)))
                   (else
                    ; This case should not be reached based on problem description's guarantee
                    ; about escape sequences.
                    (error "Unexpected escape sequence in string: " s)))))
              (else ; Regular character
               ; Consumes 1 character from code, adds 1 to memory.
               (loop (+ idx 1) (+ mem-len 1)))))))))

; --- Part 2: Calculate encoded_length - code_length ---
; This function calculates the length of a newly encoded string literal.
; The encoding rule is:
; 1. The entire original string literal (including its outer quotes) is wrapped in new double quotes.
; 2. Any existing backslash '\' or double-quote '"' characters *within* the original string
;    (including the original outer quotes) are themselves escaped with an additional backslash.
(define (calculate-encoded-length s)
  (let ((len (string-length s)))
    ; Start with 2 for the new outer quotes that wrap the entire original string.
    (let loop ((idx 0) (enc-len 2))
      (if (>= idx len)
          enc-len
          (let ((char (string-ref s idx)))
            (cond
              ((or (char=? char #\\) (char=? char #\"))
               ; A backslash or double-quote character in the original string needs to be escaped.
               ; For example, `\` becomes `\\`, `"` becomes `\"`.
               ; Each such character contributes 2 to the encoded length.
               (loop (+ idx 1) (+ enc-len 2)))
              (else
               ; A regular character contributes 1 to the encoded length.
               (loop (+ idx 1) (+ enc-len 1)))))))))

; --- Main program entry point ---
; Reads lines from "input.txt", calculates differences for Part 1 and Part 2,
; and prints the total sums.
(define (main)
  (let ((total-diff-part1 0)
        (total-diff-part2 0))
    ; Open "input.txt" for reading. `with-input-from-file` ensures the file is closed.
    (with-input-from-file "input.txt"
      (lambda ()
        ; Loop through each line of the file until EOF.
        (let loop-lines ()
          (let ((line (read-line)))
            (unless (eof-object? line) ; Stop when end-of-file is reached
              ; Process only non-empty lines, as problem implies valid string literals.
              (when (> (string-length line) 0)
                (let ((code-len (string-length line))
                      (mem-len (calculate-memory-length line))
                      (enc-len (calculate-encoded-length line)))
                  ; Accumulate differences for Part 1: (code length - memory length)
                  (set! total-diff-part1 (+ total-diff-part1 (- code-len mem-len)))
                  ; Accumulate differences for Part 2: (encoded length - code length)
                  (set! total-diff-part2 (+ total-diff-part2 (- enc-len code-len)))))
              (loop-lines)))))) ; Continue to the next line

    ; Print results to standard output.
    (display "Part 1: ")
    (display total-diff-part1)
    (newline)
    (display "Part 2: ")
    (display total-diff-part2)
    (newline)))

; Call the main function when the script is executed.
(main)
