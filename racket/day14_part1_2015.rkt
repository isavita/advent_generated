#lang racket

(define (parse-reindeer line)
  (define parts (regexp-split #px"\\s+" line))
  (define name (list-ref parts 0))
  (define speed (string->number (list-ref parts 3)))
  (define fly-time (string->number (list-ref parts 6)))
  (define rest-time (string->number (list-ref parts 13)))
  (list name speed fly-time rest-time))

(define (calculate-distance speed fly-time rest-time total-time)
  (define cycle-time (+ fly-time rest-time))
  (define full-cycles (quotient total-time cycle-time))
  (define remaining-time (modulo total-time cycle-time))
  (define distance (* full-cycles speed fly-time))
  (define extra-distance (min remaining-time fly-time))
  (+ distance (* extra-distance speed)))

(define (find-winning-reindeer reindeer total-time)
  (define distances
    (map (lambda (r)
           (apply calculate-distance (append (cdr r) (list total-time))))
         reindeer))
  (apply max distances))

(define (main)
  (define input (file->lines "input.txt"))
  (define reindeer (map parse-reindeer input))
  (define winning-distance (find-winning-reindeer reindeer 2503))
  (printf "~a\n" winning-distance))

(main)