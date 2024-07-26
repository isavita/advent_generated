
#lang racket

(define (main)
  (define input (string-trim (file->string "input.txt")))
  (define len (string-length input))
  (define sum (for/sum ([i (in-range len)])
                (define next (modulo (+ i 1) len))
                (if (char=? (string-ref input i) (string-ref input next))
                    (- (char->integer (string-ref input i)) (char->integer #\0))
                    0)))
  (displayln sum))

(main)
