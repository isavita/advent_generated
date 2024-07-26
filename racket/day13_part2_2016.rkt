
#lang racket

(define favorite-number 1362)

(define (is-wall x y)
  (define num (+ (* x x) (* 3 x) (* 2 x y) y (* y y) favorite-number))
  (define bits (count (lambda (b) (eq? (bitwise-and b 1) 1)) (in-bits num)))
  (not (even? bits)))

(define (in-bits n)
  (if (zero? n) '() (cons (bitwise-and n 1) (in-bits (arithmetic-shift n -1)))))

(define (bfs-max-steps start max-steps)
  (define visited (make-hash))
  (define queue (list start))
  (hash-set! visited start #t)
  (define steps 0)

  (define (process-queue)
    (when (and (not (null? queue)) (< steps max-steps))
      (define size (length queue))
      (for ([i (in-range size)])
        (define point (list-ref queue i))
        (for ([delta '((1 0) (-1 0) (0 1) (0 -1))])
          (define next (list (+ (first point) (first delta)) (+ (second point) (second delta))))
          (when (and (>= (first next) 0) (>= (second next) 0) (not (is-wall (first next) (second next))) (not (hash-has-key? visited next)))
            (hash-set! visited next #t)
            (set! queue (append queue (list next))))))
      (set! queue (drop queue size))
      (set! steps (add1 steps))
      (process-queue)))

  (process-queue)
  (hash-count visited))

(define start '(1 1))
(define reachable-locations (bfs-max-steps start 50))
(display reachable-locations)
