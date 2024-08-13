#lang racket

(define (find-earliest-bus filename)
  (define input (file->lines filename))
  (define earliest-time (string->number (first input)))
  (define bus-ids (filter (lambda (id) (not (string=? id "x")))
                          (string-split (second input) ",")))
  
  (define (next-departure bus-id)
    (let ((wait-time (modulo earliest-time bus-id)))
      (if (= wait-time 0)
          0
          (- bus-id wait-time))))
  
  (define (earliest-bus)
    (for/fold ([min-wait +inf.0]
               [best-bus 0])
               ([bus-id (map string->number bus-ids)])
      (let ([wait (next-departure bus-id)])
        (if (< wait min-wait)
            (values wait bus-id)
            (values min-wait best-bus)))))
  
  (define-values (wait bus-id) (earliest-bus))
  (* bus-id wait))

(define (main)
  (define result (find-earliest-bus "input.txt"))
  (printf "Result: ~a\n" result))

(main)