#lang racket

(define (process-instruction ship action value)
  (define facing (third ship))
  (cond
    [(equal? action #\N) (list (first ship) (+ (second ship) value) facing)]
    [(equal? action #\S) (list (first ship) (- (second ship) value) facing)]
    [(equal? action #\E) (list (+ (first ship) value) (second ship) facing)]
    [(equal? action #\W) (list (- (first ship) value) (second ship) facing)]
    [(equal? action #\L) (list (first ship) (second ship) (modulo (- facing value) 360))]
    [(equal? action #\R) (list (first ship) (second ship) (modulo (+ facing value) 360))]
    [(equal? action #\F)
     (case facing
       [(0) (list (+ (first ship) value) (second ship) facing)]
       [(90) (list (first ship) (- (second ship) value) facing)]
       [(180) (list (- (first ship) value) (second ship) facing)]
       [(270) (list (first ship) (+ (second ship) value) facing)])]))

(define (manhattan-distance ship)
  (+ (abs (first ship)) (abs (second ship))))

(define (main)
  (define input (file->lines "input.txt"))
  (define ship (list 0 0 0))
  (for ([line input])
    (define action (string-ref line 0))
    (define value (string->number (substring line 1)))
    (set! ship (process-instruction ship action value)))
  (display (manhattan-distance ship)))

(main)