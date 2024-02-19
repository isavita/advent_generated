
#lang racket

(define (count-orbits orbits obj)
  (if (hash-has-key? orbits obj)
      (+ 1 (count-orbits orbits (hash-ref orbits obj)))
      0))

(define orbits (make-hash))

(define input (file->lines "input.txt"))

(for-each
  (lambda (line)
    (define parts (string-split line ")"))
    (hash-set! orbits (second parts) (first parts)))
  input)

(define total-orbits 0)

(for-each
  (lambda (key)
    (set! total-orbits (+ total-orbits (count-orbits orbits key))))
  (hash-keys orbits))

(printf "Total number of direct and indirect orbits: ~a\n" total-orbits)
