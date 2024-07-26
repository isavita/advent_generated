
#lang racket

(require json)

(define (sum-numbers data)
  (cond
    [(list? data) (apply + (map sum-numbers data))]
    [(hash? data) (apply + (map sum-numbers (hash-values data)))]
    [(number? data) data]
    [else 0]))

(define (main)
  (define input (file->string "input.txt"))
  (define json-data (string->jsexpr input))
  (define result (sum-numbers json-data))
  (displayln result))

(main)
