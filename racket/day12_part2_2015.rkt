
#lang racket

(require json)

(define (sum-numbers data)
  (cond
    [(list? data) (apply + (map sum-numbers data))]
    [(hash? data) (if (contains-red? data)
                      0
                      (apply + (map sum-numbers (hash-values data))))]
    [(number? data) data]
    [else 0]))

(define (contains-red? obj)
  (for/or ([v (hash-values obj)])
    (and (string? v) (equal? v "red"))))

(define (main)
  (define data (file->string "input.txt"))
  (define json-data (string->jsexpr data))
  (define result (sum-numbers json-data))
  (displayln result))

(main)
