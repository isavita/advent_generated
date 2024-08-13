#lang racket

(define (main)
  (define input (file->string "input.txt"))
  (define lines (string-split input "\n" #:trim? #t))

  (define horizontal-position 0)
  (define depth 0)
  (define aim 0)

  (for ([line lines])
    (define command (string-split line))
    (define direction (first command))
    (define units (string->number (second command)))

    (cond
      [(equal? direction "forward")
       (set! horizontal-position (+ horizontal-position units))
       (set! depth (+ depth (* aim units)))]
      [(equal? direction "down")
       (set! aim (+ aim units))]
      [(equal? direction "up")
       (set! aim (- aim units))]))

  (displayln (* horizontal-position depth)))

(main)