#lang racket

(define (main)
  (define data (file->string "input.txt"))
  (define lines (map string-trim (string-split data "\n")))

  (define holder-map (make-hash))
  (define held-map (make-hash))
  (define re (regexp "[a-z]+"))

  (for ([line lines])
    (define names (regexp-match* re line))
    (define holder (first names))
    (hash-set! holder-map holder #t)
    (for ([name (rest names)])
      (hash-set! held-map name #t)))

  (for ([holder (hash-keys holder-map)])
    (unless (hash-has-key? held-map holder)
      (displayln holder))))

(main)