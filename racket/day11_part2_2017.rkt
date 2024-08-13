#lang racket

(define (abs x) (if (< x 0) (- x) x))
(define (max a b) (if (> a b) a b))
(define (distance x y z) (/ (+ (abs x) (abs y) (abs z)) 2))

(define (update-coordinates coords direction)
  (match direction
    ["n" (list (first coords) (+ (second coords) 1) (- (third coords) 1))]
    ["ne" (list (+ (first coords) 1) (second coords) (- (third coords) 1))]
    ["se" (list (+ (first coords) 1) (- (second coords) 1) (third coords))]
    ["s" (list (first coords) (- (second coords) 1) (+ (third coords) 1))]
    ["sw" (list (- (first coords) 1) (second coords) (+ (third coords) 1))]
    ["nw" (list (- (first coords) 1) (+ (second coords) 1) (third coords))]
    [else coords]))

(define (main)
  (define input (with-input-from-file "input.txt" (lambda () (read-line))))
  (define directions (string-split input ","))
  
  (define initial-coords (list 0 0 0))
  (define-values (x y z) (apply values initial-coords))
  (define max-distance 0)

  (for ([dir directions])
    (set! initial-coords (update-coordinates initial-coords dir))
    (define cur-distance (distance (first initial-coords) (second initial-coords) (third initial-coords)))
    (set! max-distance (max max-distance cur-distance)))

  (displayln max-distance))

(main)