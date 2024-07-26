
#lang racket

(define (is-prime n)
  (define (check-divisor i)
    (cond
      [(> (* i i) n) #t]
      [(= (modulo n i) 0) #f]
      [else (check-divisor (+ i 1))]))
  (if (< n 2) #f (check-divisor 2)))

(define (count-non-primes b c)
  (define (loop x h)
    (if (> x c)
        h
        (loop (+ x 17) (if (is-prime x) h (+ h 1)))))
  (loop b 0))

(define b (+ (* 57 100) 100000))
(define c (+ b 17000))
(define h (count-non-primes b c))

(display h)
