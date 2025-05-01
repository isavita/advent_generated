
#lang racket

(define (find-i n)
  (let loop ([i 1])
    (if (<= (* i 3) n)
        (loop (* i 3))
        i)))

(define (josephus n)
  (let ([i (find-i n)])
    (+ (- n i) (max (- n (* 2 i)) 0))))

(define (main)
  (define num-elves
    (with-input-from-file "input.txt"
      (lambda ()
        (read))))
  (display (josephus num-elves))
  (newline))

(main)
