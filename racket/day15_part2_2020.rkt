
#lang racket

(define (main)
  (define input-file "input.txt")
  (define max-turn 30000000)

  (define starting-numbers
    (with-input-from-file input-file
      (lambda ()
        (map string->number
             (string-split (string-trim (read-line (current-input-port) 'any)) ",")))))

  (define spoken (make-hash))
  (define last-spoken #f)

  (for ([number starting-numbers]
        [turn (in-naturals 1)])
    (when last-spoken
      (hash-set! spoken last-spoken (- turn 1)))
    (set! last-spoken number))

  (for ([turn (in-range (+ (length starting-numbers) 1) (+ max-turn 1))])
    (define next-number
      (if (hash-has-key? spoken last-spoken)
          (- (- turn 1) (hash-ref spoken last-spoken))
          0))
    (hash-set! spoken last-spoken (- turn 1))
    (set! last-spoken next-number))

  (print last-spoken))

(main)
