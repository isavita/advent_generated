
#lang racket

(define (count-letters str)
  (define count (make-hash))
  (for ([c (string->list str)])
    (hash-update! count c add1 0))
  count)

(define (calculate-checksum boxes)
  (define (count-occurrences count)
    (define has-2? #f)
    (define has-3? #f)
    (for/and ([(key val) (in-hash count)])
      (cond
        [(= val 2) (set! has-2? #t)]
        [(= val 3) (set! has-3? #t)]))
    (values has-2? has-3?))
  
  (define (count-boxes boxes has-2 has-3)
    (define count-2 0)
    (define count-3 0)
    (for ([box boxes])
      (define count (count-letters box))
      (define-values (has-2? has-3?) (count-occurrences count))
      (when has-2? (set! count-2 (add1 count-2)))
      (when has-3? (set! count-3 (add1 count-3)))
    )
    (* count-2 count-3))
  
  (count-boxes boxes #f #f))

(define input (file->lines "input.txt"))
(displayln (calculate-checksum input))
