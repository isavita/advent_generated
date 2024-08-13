#lang racket

(define (transform-subject subject loop-size)
  (define (loop value iterations)
    (if (= iterations 0)
        value
        (loop (modulo (* value subject) 20201227) (sub1 iterations))))
  (loop 1 loop-size))

(define (find-loop-size public-key)
  (define (search value loop-size)
    (if (= value public-key)
        loop-size
        (search (modulo (* value 7) 20201227) (add1 loop-size))))
  (search 1 0))

(define (compute-encryption-key public-key loop-size)
  (transform-subject public-key loop-size))

(define (main)
  (define input (file->lines "input.txt"))
  (define card-public-key (string->number (first input)))
  (define door-public-key (string->number (second input)))

  (define card-loop-size (find-loop-size card-public-key))
  (define encryption-key (compute-encryption-key door-public-key card-loop-size))
  (printf "Encryption key: ~a\n" encryption-key))

(main)