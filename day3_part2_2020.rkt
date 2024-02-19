
#lang racket

(define (count-trees grid right down)
  (define height (length grid))
  (define width (string-length (car grid)))
  
  (define (check x y)
    (define row (list-ref grid y))
    (define col (string-ref row (remainder x width)))
    (if (char=? col #\#) 1 0))
  
  (define (helper x y count)
    (cond
      [(>= y height) count]
      [else
       (helper (+ x right) (+ y down) (+ count (check x y)))]))
  
  (helper 0 0 0))

(define (main)
  (define input (file->lines "input.txt"))
  
  (define result1 (count-trees input 3 1))
  (define result2 (* result1
                    (count-trees input 1 1)
                    (count-trees input 5 1)
                    (count-trees input 7 1)
                    (count-trees input 1 2)))
  
  (displayln result1)
  (displayln result2))

(main)
