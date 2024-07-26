
#lang racket

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ([line (in-lines)])
        (map string->number (string-split line ","))))))

(define (calculate-exposed-sides p cubes)
  (define directions '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0) (0 0 1) (0 0 -1)))
  (define exposed-sides 6)
  (for ([dir directions])
    (define adjacent (map + p dir))
    (when (hash-ref cubes adjacent #f)
      (set! exposed-sides (sub1 exposed-sides))))
  exposed-sides)

(define (main)
  (define cubes (make-hash))
  (for ([coords (read-input "input.txt")])
    (hash-set! cubes coords #t))
  
  (define surface-area
    (apply + (map (lambda (cube) (calculate-exposed-sides cube cubes)) (hash-keys cubes))))
  
  (displayln surface-area))

(main)
