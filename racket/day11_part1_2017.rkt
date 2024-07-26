
#lang racket

(define (abs x) (if (< x 0) (- x) x))
(define (max a b) (if (> a b) a b))
(define (distance x y z) (/ (+ (abs x) (abs y) (abs z)) 2))

(define (parse-input)
  (with-input-from-file "input.txt"
    (lambda () (read-line))))

(define (move dir x y z)
  (cond
    [(equal? dir "n") (values x (+ y 1) (- z 1))]
    [(equal? dir "ne") (values (+ x 1) y (- z 1))]
    [(equal? dir "se") (values (+ x 1) (- y 1) z)]
    [(equal? dir "s") (values x (- y 1) (+ z 1))]
    [(equal? dir "sw") (values (- x 1) y (+ z 1))]
    [(equal? dir "nw") (values (- x 1) (+ y 1) z)]))

(define (main)
  (define input (parse-input))
  (define directions (string-split input ","))
  
  (define-values (x y z) (values 0 0 0))
  (define max-distance 0)

  (for ([dir directions])
    (define-values (new-x new-y new-z) (move dir x y z))
    (set! x new-x)
    (set! y new-y)
    (set! z new-z)
    (set! max-distance (max max-distance (distance x y z))))

  (printf "~a\n" (distance x y z)))

(main)
