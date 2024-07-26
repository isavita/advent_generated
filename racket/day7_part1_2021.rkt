
#lang racket

(define (calculate-fuel current new)
  (abs (- current new)))

(define (min-fuel positions)
  (define min-pos (apply min positions))
  (define max-pos (apply max positions))
  (define (fuel-cost target)
    (apply + (map (lambda (pos) (calculate-fuel pos target)) positions)))
  (define fuel-list (map fuel-cost (range min-pos (+ max-pos 1))))
  (apply min fuel-list))

(define (main)
  (define input (file->string "input.txt"))
  (define positions (map string->number (string-split input ",")))
  (printf "~a\n" (min-fuel positions)))

(main)
