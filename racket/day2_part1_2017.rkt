
#lang racket

(define (calculate-checksum rows)
  (define (row-checksum row)
    (- (apply max row) (apply min row)))
  
  (apply + (map row-checksum rows)))

(define input (file->lines "input.txt"))
(define rows (map (λ (row) (map string->number (string-split row))) input))

(printf "~a\n" (calculate-checksum rows))
