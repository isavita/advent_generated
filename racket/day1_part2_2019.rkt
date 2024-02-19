
#lang racket
(define (calculate-fuel mass)
  (max 0 (- (quotient mass 3) 2))
  )

(define (calculate-total-fuel mass)
  (let loop ((fuel (calculate-fuel mass))
             (total-fuel 0))
    (if (<= fuel 0)
        total-fuel
        (loop (calculate-fuel fuel) (+ total-fuel fuel))
        )
    )
  )

(define (sum-fuel-requirements file)
  (let ((input (file->lines file)))
    (apply + (map (λ (x) (calculate-total-fuel (string->number x))) input))
    )
  )

(displayln (sum-fuel-requirements "input.txt"))
