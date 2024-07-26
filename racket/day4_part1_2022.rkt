
#lang racket

(define (parse-range r)
  (map string->number (string-split r "-")))

(define (count-overlapping-ranges filename)
  (define (overlaps? r1 r2)
    (or (and (<= (first r1) (first r2)) (>= (second r1) (second r2)))
        (and (<= (first r2) (first r1)) (>= (second r2) (second r1)))))
  
  (define count 0)
  (for ([line (in-lines (open-input-file filename))])
    (define ranges (map parse-range (string-split line ",")))
    (when (= (length ranges) 2)
      (when (overlaps? (first ranges) (second ranges))
        (set! count (add1 count)))))
  count)

(printf "~a\n" (count-overlapping-ranges "input.txt"))
