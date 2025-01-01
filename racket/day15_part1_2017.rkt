
#lang racket

(define (solve)
  (define in (open-input-file "input.txt"))
  (define genA-start (string->number (read-line in)))
  (define genB-start (string->number (read-line in)))
  (close-input-port in)

  (define genA-factor 16807)
  (define genB-factor 48271)
  (define modulus 2147483647)

  (let loop ([genA genA-start] [genB genB-start] [matches 0] [i 0])
    (if (= i 40000000)
        matches
        (let* ([next-genA (remainder (* genA genA-factor) modulus)]
               [next-genB (remainder (* genB genB-factor) modulus)]
               [new-matches (if (= (bitwise-and next-genA #xFFFF) (bitwise-and next-genB #xFFFF))
                                (+ matches 1)
                                matches)])
          (loop next-genA next-genB new-matches (+ i 1))))))

(printf "~a\n" (solve))
