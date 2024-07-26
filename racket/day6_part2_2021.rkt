
#lang racket

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (define line (read-line))
      (map string->number (string-split line ",")))))

(define (simulate-fish lantern-fish-counts days)
  (for ([i (in-range days)])
    (let ([new-fish (vector-ref lantern-fish-counts 0)])
      (for ([j (in-range 8)])
        (vector-set! lantern-fish-counts j (vector-ref lantern-fish-counts (+ j 1))))
      (vector-set! lantern-fish-counts 6 (+ (vector-ref lantern-fish-counts 6) new-fish))
      (vector-set! lantern-fish-counts 8 new-fish)))
  (apply + (vector->list lantern-fish-counts)))

(define (main)
  (define fish-ages (read-input "input.txt"))
  (define lantern-fish-counts (make-vector 9 0))
  (for ([age fish-ages])
    (vector-set! lantern-fish-counts age (+ (vector-ref lantern-fish-counts age) 1)))
  (printf "~a\n" (simulate-fish lantern-fish-counts 256)))

(main)
