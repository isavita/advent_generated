
#lang racket

(define (count-increases lst)
  (define (helper lst prev acc)
    (cond
      [(empty? lst) acc]
      [(< (first lst) prev) (helper (rest lst) (first lst) acc)]
      [else (helper (rest lst) (first lst) (+ acc 1))]))
  (helper (rest lst) (first lst) 0))

(define input (file->lines "input.txt"))
(define depths (map string->number input))

(printf "~a\n" (count-increases depths))
