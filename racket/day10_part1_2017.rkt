
#lang racket

(define (read-input file)
  (with-input-from-file file
    (lambda ()
      (map string->number (string-split (read-line) ",")))))

(define (knot-hash lengths)
  (define list (build-vector 256 (lambda (i) i)))
  (define current-position 0)
  (define skip-size 0)

  (for ([length lengths])
    (for ([i (in-range (quotient length 2))])
      (define start (modulo (+ current-position i) 256))
      (define end (modulo (+ current-position (- length 1 i)) 256))
      (define temp (vector-ref list start))
      (vector-set! list start (vector-ref list end))
      (vector-set! list end temp))
    (set! current-position (modulo (+ current-position length skip-size) 256))
    (set! skip-size (+ skip-size 1)))

  (* (vector-ref list 0) (vector-ref list 1)))

(define lengths (read-input "input.txt"))
(printf "~a\n" (knot-hash lengths))
