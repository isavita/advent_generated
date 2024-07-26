
#lang racket

(define Clean 0)
(define Weakened 1)
(define Infected 2)
(define Flagged 3)

(define (read-input filename)
  (define grid (make-hash))
  (define start-x 0)
  (define start-y 0)
  (define y 0)
  (for ([line (in-lines (open-input-file filename))])
    (for ([x (in-range (string-length line))])
      (when (char=? (string-ref line x) #\#)
        (hash-set! grid (cons x y) Infected)))
    (set! start-x (max start-x (string-length line)))
    (set! start-y (add1 y))
    (set! y (add1 y)))
  (values grid (floor (/ start-x 2)) (floor (/ start-y 2))))

(define (simulate grid start-x start-y iterations)
  (define dx '(0 1 0 -1))
  (define dy '(-1 0 1 0))
  (define x start-x)
  (define y start-y)
  (define dir 0)
  (define infected-count 0)

  (for ([i (in-range iterations)])
    (define pos (cons x y))
    (define state (hash-ref grid pos Clean))
    (cond
      [(= state Clean)
       (set! dir (modulo (- dir 1) 4))
       (hash-set! grid pos Weakened)]
      [(= state Weakened)
       (set! infected-count (add1 infected-count))
       (hash-set! grid pos Infected)]
      [(= state Infected)
       (set! dir (modulo (+ dir 1) 4))
       (hash-set! grid pos Flagged)]
      [(= state Flagged)
       (set! dir (modulo (+ dir 2) 4))
       (hash-set! grid pos Clean)])
    (set! x (+ x (list-ref dx dir)))
    (set! y (+ y (list-ref dy dir))))

  infected-count)

(define-values (grid start-x start-y) (read-input "input.txt"))
(define result (simulate grid start-x start-y 10000000))
(displayln result)
