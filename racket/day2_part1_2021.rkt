
#lang racket

(define (calculate-position file)
  (define input (file->lines file))
  (define horizontal 0)
  (define depth 0)
  
  (for-each
    (lambda (line)
      (define parts (string-split line " "))
      (define direction (first parts))
      (define amount (string->number (second parts)))
      
      (cond
        [(string=? direction "forward") (set! horizontal (+ horizontal amount))]
        [(string=? direction "down") (set! depth (+ depth amount))]
        [(string=? direction "up") (set! depth (- depth amount))]
      ))
    input)
  
  (* horizontal depth))

(displayln (calculate-position "input.txt"))
