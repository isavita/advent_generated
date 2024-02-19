
#lang racket

(define (parse-coords str)
  (map string->number (string-split str ",")))

(define (parse-line line)
  (map parse-coords (string-split line " -> ")))

(define (create-grid lines)
  (define grid (make-hash))
  (for ([line lines])
    (define start (first line))
    (define end (second line))
    (define x1 (first start))
    (define y1 (second start))
    (define x2 (first end))
    (define y2 (second end))
    (cond
      [(= x1 x2)
       (when (> y1 y2)
         (set! y1 y2)
         (set! y2 (second start)))
       (for ([y (in-range y1 (add1 y2))])
         (hash-update! grid (list x1 y) add1 0))]
      [(= y1 y2)
       (when (> x1 x2)
         (set! x1 x2)
         (set! x2 (first start)))
       (for ([x (in-range x1 (add1 x2))])
         (hash-update! grid (list x y1) add1 0))]))
  grid)

(define (count-overlaps grid)
  (define overlap-count 0)
  (for/sum ([val (in-hash-values grid)])
    (if (> val 1) 1 0)))

(define (main)
  (define input (file->lines "input.txt"))
  (define lines (map parse-line input))
  (define grid (create-grid lines))
  (define overlap-count (count-overlaps grid))
  (display overlap-count))

(main)
