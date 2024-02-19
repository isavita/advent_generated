
#lang racket

(define (move x y direction)
  (cond
    [(equal? direction "^") (list x (+ y 1))]
    [(equal? direction "v") (list x (- y 1))]
    [(equal? direction ">") (list (+ x 1) y)]
    [(equal? direction "<") (list (- x 1) y)]))

(define (deliver-presents directions)
  (define visited-houses (list (list 0 0)))
  (define santa-pos (list 0 0))
  (define robo-santa-pos (list 0 0))
  (define santa-turn #t)
  
  (for ([direction (string->list directions)])
    (if santa-turn
        (begin
          (set! santa-pos (move (car santa-pos) (cadr santa-pos) (string direction)))
          (unless (member santa-pos visited-houses) (set! visited-houses (cons santa-pos visited-houses))))
        (begin
          (set! robo-santa-pos (move (car robo-santa-pos) (cadr robo-santa-pos) (string direction)))
          (unless (member robo-santa-pos visited-houses) (set! visited-houses (cons robo-santa-pos visited-houses))))
        )
    (set! santa-turn (not santa-turn)))
  
  (display (length visited-houses)))

(define input (file->lines "input.txt"))
(deliver-presents (car input))
