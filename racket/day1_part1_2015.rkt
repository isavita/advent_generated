#lang racket

; Read input from input.txt
(define input (call-with-input-file "input.txt" read-line))

; Function to calculate the final floor
(define (final-floor instructions)
  (define floor 0)
  (for-each
   (lambda (char)
     (if (char=? char #\()
         (set! floor (+ floor 1))
         (set! floor (- floor 1))))
   (string->list instructions))
  floor)

; Get the final floor
(define result (final-floor input))
(display result)
