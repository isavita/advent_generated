
#lang racket

(define (solve)
  (define target
    (let* ([input-str (string-trim (port->string (open-input-file "input.txt")))]
           [target-num (string->number input-str)])
      (if target-num
          (quotient target-num 10)
          (error "Invalid input"))))

  (define houses (make-vector (+ target 1) 0))

  (for ([elf (in-range 1 (+ target 1))])
    (for ([house (in-range elf (+ target 1) elf)])
      (vector-set! houses house (+ (vector-ref houses house) elf))))

  (for ([house-number (in-naturals)]
        [presents (in-vector houses)])
    (when (>= presents target)
      (printf "~a\n" house-number)
      (exit))))

(solve)
