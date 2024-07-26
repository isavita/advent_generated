
#lang racket

(define (read-input filename)
  (define grid (make-hash))
  (define start-x 0)
  (define start-y 0)
  (define lines (file->lines filename))
  (for ([y (in-range (length lines))])
    (for ([x (in-range (string-length (list-ref lines y)))])
      (let ([c (string-ref (list-ref lines y) x)])
        (when (equal? c #\#)
          (hash-set! grid (cons x y) #t))))
    (set! start-x (quotient (string-length (list-ref lines y)) 2))
    (set! start-y (quotient y 2)))
  (values grid start-x start-y))

(define (simulate grid start-x start-y iterations)
  (define dx '(0 1 0 -1))
  (define dy '(-1 0 1 0))
  (define x start-x)
  (define y start-y)
  (define dir 0)
  (define infected-count 0)

  (for ([i (in-range iterations)])
    (define pos (cons x y))
    (if (hash-ref grid pos #f)
        (begin
          (set! dir (modulo (+ dir 1) 4))
          (hash-remove! grid pos))
        (begin
          (set! dir (modulo (- dir 1) 4))
          (hash-set! grid pos #t)
          (set! infected-count (+ infected-count 1))))
    (set! x (+ x (list-ref dx dir)))
    (set! y (+ y (list-ref dy dir))))

  infected-count)

(define-values (grid start-x start-y) (read-input "input.txt"))
(define result (simulate grid start-x start-y 10000))
(displayln result)
