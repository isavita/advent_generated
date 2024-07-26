
#lang racket

(define (read-input file)
  (with-input-from-file file
    (lambda ()
      (read-line))))

(define (count-fish input)
  (define fishes (make-vector 9 0))
  (for ([fish (map string->number (string-split input ","))])
    (vector-set! fishes fish (+ (vector-ref fishes fish) 1)))
  fishes)

(define (simulate-fish-growth fishes days)
  (for ([day (in-range days)])
    (define new-fish (vector-ref fishes 0))
    (for ([i (in-range 1 9)])
      (vector-set! fishes (- i 1) (vector-ref fishes i)))
    (vector-set! fishes 6 (+ (vector-ref fishes 6) new-fish))
    (vector-set! fishes 8 new-fish))
  fishes)

(define (total-fish fishes)
  (apply + (vector->list fishes)))

(define (main)
  (define input (read-input "input.txt"))
  (define fishes (count-fish input))
  (define final-fishes (simulate-fish-growth fishes 80))
  (printf "~a\n" (total-fish final-fishes)))

(main)
