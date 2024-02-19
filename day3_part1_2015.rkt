
#lang racket

(define (move x y dir)
  (cond
    [(char=? dir #\^) (cons x (add1 y))]
    [(char=? dir #\v) (cons x (sub1 y))]
    [(char=? dir #\>) (cons (add1 x) y)]
    [(char=? dir #\<) (cons (sub1 x) y)]
    [else (cons x y)]))

(define (count-houses input)
  (define visited (make-hash))
  (define x 0)
  (define y 0)
  (hash-set! visited (cons x y) #t)
  (for-each
    (lambda (dir)
      (set! x (car (move x y dir)))
      (set! y (cdr (move x y dir)))
      (hash-set! visited (cons x y) #t))
    input)
  (hash-count visited))

(define input (file->lines "input.txt"))
(printf "Number of houses visited: ~a\n" (count-houses (string->list (car input))))
