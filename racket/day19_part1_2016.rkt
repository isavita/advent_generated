
#lang racket

(define (main)
  (define total-elves (read-input "input.txt"))
  (define winner (find-winning-elf total-elves))
  (displayln winner))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read))))

(define (find-winning-elf total-elves)
  (define highest-power-of-two (expt 2 (floor (log total-elves 2))))
  (+ (* 2 (- total-elves highest-power-of-two)) 1))

(main)
