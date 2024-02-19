
#lang racket

(define (valid-passphrase? passphrase)
  (let ([words (string-split passphrase)])
    (= (length words) (length (remove-duplicates words)))))

(define (count-valid-passphrases passphrases)
  (length (filter valid-passphrase? passphrases)))

(define input (file->lines "input.txt"))
(displayln (count-valid-passphrases input))
