
#lang racket

(define (count-trees grid right down)
  (let loop ([x 0] [y 0] [count 0])
    (cond
      [(>= y (length grid)) count]
      [(char=? (string-ref (list-ref grid y) x) #\#)
       (loop (modulo (+ x right) (string-length (list-ref grid 0))) (+ y down) (add1 count))]
      [else
       (loop (modulo (+ x right) (string-length (list-ref grid 0))) (+ y down) count)])))

(define input (file->lines "input.txt"))
(display (count-trees input 3 1))
