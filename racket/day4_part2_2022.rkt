
#lang racket

(define (parse-range s)
  (map string->number (string-split s "-")))

(define (ranges-overlap? left right)
  (and (<= (first left) (second right)) (>= (second left) (first right))))

(define (count-overlapping-ranges filename)
  (define in (open-input-file filename))
  (define count (for/fold ([c 0]) ([line (in-lines in)])
                 (define pair (string-split line ","))
                 (define left (parse-range (first pair)))
                 (define right (parse-range (second pair)))
                 (if (ranges-overlap? left right)
                     (add1 c)
                     c)))
  (close-input-port in)
  count)

(displayln (count-overlapping-ranges "input.txt"))
