
#lang racket

(define (calculate-paper-dimensions l w h)
  (define side1 (* l w))
  (define side2 (* w h))
  (define side3 (* h l))
  (define smallest-side (apply min (list side1 side2 side3)))
  (+ (* 2 l w) (* 2 w h) (* 2 h l) smallest-side))

(define (main)
  (define input (file->lines "input.txt"))
  (define total-paper 0)
  (for ([line input])
    (define dimensions (map string->number (string-split line "x")))
    (set! total-paper (+ total-paper (calculate-paper-dimensions (first dimensions) (second dimensions) (third dimensions)))))
  (display total-paper))

(main)
