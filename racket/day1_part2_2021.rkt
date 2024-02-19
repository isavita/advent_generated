
#lang racket

(define (read-input file)
  (call-with-input-file file
    (lambda (in)
      (let loop ([vals '()] [line (read-line in)])
        (cond
          [(eof-object? line) (reverse vals)]
          [(string=? line "") (loop vals (read-line in))]
          [else (loop (cons (string->number line) vals) (read-line in))])))))

(define (main)
  (define input (read-input "input.txt"))
  
  (define prev-sum (+ (first input) (second input) (third input)))
  (define count 0)
  
  (for ([i (in-range 3 (length input))])
    (define curr-sum (+ (list-ref input (- i 2)) (list-ref input (- i 1)) (list-ref input i)))
    (when (> curr-sum prev-sum)
      (set! count (+ count 1)))
    (set! prev-sum curr-sum))
  
  (displayln count))

(main)
