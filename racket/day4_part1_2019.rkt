
#lang racket

(define (has-double-and-increasing-digits s)
  (define len (string-length s))
  (define has-double #f)
  (define increasing #t)
  (for ([i (in-range (- len 1))])
    (when (equal? (string-ref s i) (string-ref s (+ i 1)))
      (set! has-double #t))
    (when (> (char->integer (string-ref s i)) (char->integer (string-ref s (+ i 1))))
      (set! increasing #f)))
  (and has-double increasing))

(define (count-valid-passwords start end)
  (define count 0)
  (for ([i (in-range start (+ end 1))])
    (when (has-double-and-increasing-digits (number->string i))
      (set! count (+ count 1))))
  count)

(define (main)
  (define input (file->string "input.txt"))
  (define parts (map string->number (string-split (string-trim input) "-")))
  (define start (first parts))
  (define end (second parts))
  (displayln (count-valid-passwords start end)))

(main)
