
#lang racket

(define (solve)
  (define target
    (string->number (string-trim (file->string "input.txt"))))
  (define target/11 (quotient target 11))
  (define houses (make-vector (+ target/11 1) 0))
  
  (for ([elf (in-range 1 (+ target/11 1))])
    (for ([house (in-range elf (+ target/11 1) elf)]
          #:when (<= house (* elf 50)))
      (vector-set! houses house (+ (vector-ref houses house) elf))))

  (for ([house-number (in-naturals)]
        [presents (in-vector houses)])
    (when (>= presents target/11)
      (printf "~a\n" house-number)
      (exit))))

(solve)
