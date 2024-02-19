
#lang racket

(define (read-input file)
  (call-with-input-file file
    (lambda (in)
      (read-line in))))

(define (solve-captcha input)
  (define n (string-length input))
  (define half-n (/ n 2))
  (define sum 0)
  (for ([i (in-range n)])
    (define curr (string-ref input i))
    (define next (string-ref input (modulo (+ i half-n) n)))
    (when (char=? curr next)
      (set! sum (+ sum (- (char->integer #\0) (char->integer curr))))))
  sum)

(define input (read-input "input.txt"))
(printf "Part 1: ~a\n" (solve-captcha input))
