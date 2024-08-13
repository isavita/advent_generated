#lang racket

(define (count-arrangements adapters)
  (define ways (make-hash))
  (hash-set! ways 0 1)
  (for ([i (in-range 1 (length adapters))])
    (define current-joltage (list-ref adapters i))
    (for ([diff (list 1 2 3)])
      (when (hash-has-key? ways (- current-joltage diff))
        (hash-set! ways current-joltage 
                   (+ (hash-ref ways current-joltage 0) 
                      (hash-ref ways (- current-joltage diff) 0))))))
  (hash-ref ways (last adapters)))

(define (main)
  (define adapters (list 0))
  (define input (file->lines "input.txt"))
  (for ([line input])
    (set! adapters (cons (string->number line) adapters)))
  (define sorted-adapters (sort adapters <))
  (set! sorted-adapters (append sorted-adapters (list (+ (last sorted-adapters) 3))))
  (printf "~a\n" (count-arrangements sorted-adapters)))

(main)