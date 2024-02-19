
#lang racket

(define (turn direction current)
  (cond
    [(equal? direction "R") (cond
                               [(equal? current 'N) 'E]
                               [(equal? current 'E) 'S]
                               [(equal? current 'S) 'W]
                               [(equal? current 'W) 'N])]
    [(equal? direction "L") (cond
                               [(equal? current 'N) 'W]
                               [(equal? current 'W) 'S]
                               [(equal? current 'S) 'E]
                               [(equal? current 'E) 'N])]))

(define (move direction steps current)
  (cond
    [(equal? current 'N) (cons 0 steps)]
    [(equal? current 'E) (cons steps 0)]
    [(equal? current 'S) (cons 0 (- steps))]
    [(equal? current 'W) (cons (- steps) 0)]))

(define (distance x y)
  (+ (abs x) (abs y)))

(define (main)
  (define input (file->lines "input.txt"))
  (define instructions (string-split (first input) ", "))
  (define current-direction 'N)
  (define current-x 0)
  (define current-y 0)
  
  (for-each
   (lambda (instruction)
     (define direction (substring instruction 0 1))
     (define steps (string->number (substring instruction 1)))
     (set! current-direction (turn direction current-direction))
     (define new-coords (move direction steps current-direction))
     (set! current-x (+ current-x (car new-coords)))
     (set! current-y (+ current-y (cdr new-coords))))
   instructions)
  
  (displayln (distance current-x current-y)))

(main)
