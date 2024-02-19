
#lang racket

(define (find-multiply input)
  (let loop ([lst input])
    (define num1 (car lst))
    (define num2 (findf (lambda (x) (= (+ num1 x) 2020)) (cdr lst)))
    (if num2
        (* num1 num2)
        (loop (cdr lst)))))

(define input (file->lines "input.txt"))
(define expenses (map string->number input))
(printf "~a\n" (find-multiply expenses))
