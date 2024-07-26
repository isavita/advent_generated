
#lang racket

(define (read-input)
  (define input (file->string "input.txt"))
  (define lines (string-split input "\n" #:trim? #t))
  (list (string->number (first lines)) (string->number (second lines))))

(define (generate-value gen factor modulus)
  (modulo (* gen factor) modulus))

(define (next-gen-a genA)
  (let loop ([genA genA])
    (let ([next (generate-value genA 16807 2147483647)])
      (if (= (modulo next 4) 0)
          next
          (loop next)))))

(define (next-gen-b genB)
  (let loop ([genB genB])
    (let ([next (generate-value genB 48271 2147483647)])
      (if (= (modulo next 8) 0)
          next
          (loop next)))))

(define (count-matches genAStart genBStart iterations)
  (define (loop genA genB matches count)
    (if (= count iterations)
        matches
        (let* ([newGenA (next-gen-a genA)]
               [newGenB (next-gen-b genB)]
               [matches (if (= (bitwise-and newGenA 65535) (bitwise-and newGenB 65535))
                            (+ matches 1)
                            matches)])
          (loop newGenA newGenB matches (+ count 1)))))
  (loop genAStart genBStart 0 0))

(define (main)
  (define inputs (read-input))
  (define genAStart (first inputs))
  (define genBStart (second inputs))
  (define matches (count-matches genAStart genBStart 5000000))
  (displayln matches))

(main)
