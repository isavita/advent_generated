
#lang racket

(require racket/string) ; Required for string-append*

; Function to perform one step of the look-and-say process
; Takes a string of digits and returns the next string in the sequence.
; It iterates through the input string, identifying runs of identical characters,
; and builds a list of string parts ("count" and "digit") in reverse order
; for efficient final string construction using string-append*.
(define (look-and-say s)
  (if (string=? s "")
      ""
      (let loop ((index 1)
                 (current-char (string-ref s 0))
                 (current-count 1)
                 (result-parts '())) ; List of strings, built in reverse order (char count ... char count)
        (if (< index (string-length s))
            (let ((char-at-index (string-ref s index)))
              (if (char=? char-at-index current-char)
                  ;; Same character as the current run, increment count
                  (loop (add1 index) current-char (add1 current-count) result-parts)
                  ;; Different character, the current run ends.
                  ;; Add the count and the character of the finished run to the result list (in reverse order for later reversal)
                  ;; Then start a new run with the current character.
                  (loop (add1 index)
                        char-at-index
                        1 ; Start count for the new run
                        (cons (string current-char) (cons (number->string current-count) result-parts))))) ; Cons char-string then count-string for reversed list (becomes count-string then char-string after reverse)
            ;; End of the input string.
            ;; Add the count and the character of the final run to the result list
            (string-append* (reverse (cons (string current-char) (cons (number->string current-count) result-parts)))))))) ; Cons char-string then count-string for reversed list

; Function to apply the look-and-say process a specified number of times.
; Takes the initial string and the number of iterations.
; Uses a simple loop to repeatedly call the look-and-say function.
(define (repeat-look-and-say initial-string times)
  (let loop ((n times)
             (current-string initial-string))
    (if (zero? n)
        current-string
        (loop (sub1 n) (look-and-say current-string)))))

; Main function to handle file input and output.
; Reads the starting string from input.txt, performs the look-and-say
; process 40 times, and prints the length of the final string.
(define (main)
  ; Read the initial string from input.txt
  (define initial-string
    (call-with-input-file "input.txt"
      (lambda (port)
        (read-line port)))) ; read-line reads one line and removes the newline character

  ; Apply the look-and-say process 40 times
  (define final-string (repeat-look-and-say initial-string 40))

  ; Print the length of the resulting string
  (displayln (string-length final-string)))

;; Call the main function to execute the program
(main)
