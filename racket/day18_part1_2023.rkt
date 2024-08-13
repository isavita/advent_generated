#lang racket

(define (read-file filename)
  (with-input-from-file filename
    (lambda () (for/list ([line (in-lines)]) line))))

(define (parse-input input)
  (define directions '((#\U . (0 -1)) (#\L . (-1 0)) (#\D . (0 1)) (#\R . (1 0))))
  (define current (list 0 0))
  (define vertices (list current))
  (for ([line input])
    (define parts (string-split line))
    (define dir (string-ref (first parts) 0))
    (define length (string->number (second parts)))
    (define move (cdr (assoc dir directions)))
    (set! current (list (+ (first current) (* (first move) length))
                        (+ (second current) (* (second move) length))))
    (set! vertices (cons current vertices)))
  (reverse vertices))

(define (abs x) (if (< x 0) (- x) x))

(define (shoelace vertices)
  (define n (length vertices))
  (define area 0)
  (for ([i (in-range n)])
    (define next (modulo (+ i 1) n))
    (set! area (+ area (* (first (list-ref vertices i)) (second (list-ref vertices next)))
                        (- (* (second (list-ref vertices i)) (first (list-ref vertices next)))))))
  (/ (abs area) 2))

(define (perimeter vertices)
  (define n (length vertices))
  (define perim 0)
  (for ([i (in-range n)])
    (define next (modulo (+ i 1) n))
    (set! perim (+ perim (abs (- (first (list-ref vertices i)) (first (list-ref vertices next))))
                        (abs (- (second (list-ref vertices i)) (second (list-ref vertices next)))))))
  perim)

(define (calculate-polygon-area vertices)
  (+ (shoelace vertices) (/ (perimeter vertices) 2) 1))

(define (solve input)
  (define vertices (parse-input input))
  (calculate-polygon-area vertices))

(define input (read-file "input.txt"))
(displayln (solve input))