
#lang racket

(define (calculate-paper-dimensions l w h)
  (define lw (* l w))
  (define wh (* w h))
  (define hl (* h l))
  (+ (* 2 lw) (* 2 wh) (* 2 hl) (apply min (list lw wh hl))))

(define (calculate-ribbon-dimensions l w h)
  (define sorted-dimensions (sort (list l w h) <))
  (define smallest-perimeter (* 2 (+ (first sorted-dimensions) (second sorted-dimensions))))
  (define volume (* l w h))
  (+ smallest-perimeter volume))

(define (main)
  (define input (file->lines "input.txt"))
  (define total-paper 0)
  (define total-ribbon 0)
  (for ([line input])
    (define dimensions (map string->number (string-split line "x")))
    (set! total-paper (+ total-paper (calculate-paper-dimensions (first dimensions) (second dimensions) (third dimensions))))
    (set! total-ribbon (+ total-ribbon (calculate-ribbon-dimensions (first dimensions) (second dimensions) (third dimensions))))
  )
  (displayln total-paper)
  (displayln total-ribbon)
)

(main)
