
#lang racket

(define (count-special-digits filename)
  (define input (file->lines filename))
  (define (count-line line)
    (define output (second (string-split line " | ")))
    (define digits (string-split output " "))
    (length (filter (lambda (digit) (or (= (string-length digit) 2)
                                         (= (string-length digit) 4)
                                         (= (string-length digit) 3)
                                         (= (string-length digit) 7)))
                    digits)))
  (apply + (map count-line input)))

(define (file->lines filename)
  (with-input-from-file filename
    (lambda () (for/list ([line (in-lines)]) line))))

(define total (count-special-digits "input.txt"))
(displayln total)
