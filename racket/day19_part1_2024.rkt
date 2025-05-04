
#lang racket

(require racket/port) ; Required for port->lines

;; Checks if a design string can be segmented into a sequence of patterns.
;; Uses dynamic programming.
(define (can-make? design patterns)
  (define n (string-length design))
  ; dp vector: dp[i] is true if the first i characters of design can be made.
  (define dp (make-vector (+ n 1) #f))
  (vector-set! dp 0 #t) ; Base case: empty string is possible

  (for ([i (in-range 1 (+ n 1))]) ; Iterate through possible end positions in design
    ; Check if any pattern ending at position i results in a valid segmentation
    (define found-match?
      (for/or ([p (in-list patterns)]) ; Iterate through available patterns
        (define lp (string-length p))
        (and (>= i lp)                 ; Check if pattern fits
             (vector-ref dp (- i lp)) ; Check if the prefix before the pattern is valid
             (string=? (substring design (- i lp) i) p)))) ; Check if pattern matches suffix
    (when found-match?
      (vector-set! dp i #t))) ; Mark this position as reachable

  (vector-ref dp n)) ; Return whether the whole string is reachable

;; Main entry point
(define (main)
  (call-with-input-file "input.txt"
    (lambda (in)
      ; Read and parse the available patterns from the first line
      (define patterns-line (read-line in))
      (define available-patterns
        (map string-trim (string-split patterns-line ",")))

      ; Skip the second line
      (read-line in)

      ; Process the remaining lines (designs) and count valid ones
      (define count
        (for/fold ([current-count 0]) ; Accumulate the count
                  ([design (in-lines in)]) ; Iterate through remaining lines
          (define trimmed-design (string-trim design))
          (if (can-make? trimmed-design available-patterns)
              (+ current-count 1)
              current-count)))

      ; Print the final count
      (display count)
      (newline))))

; Run the main function
(main)
