
#lang racket

(define (main)
  (define steps (string->number (string-trim (file->string "input.txt"))))
  (define current-pos 0)
  (define value-after-zero 0)

  (for ([i (in-range 1 50000001)])
    (set! current-pos (modulo (+ current-pos steps) i))
    (when (= current-pos 0)
      (set! value-after-zero i))
    (set! current-pos (add1 current-pos)))

  (displayln value-after-zero))

(main)
