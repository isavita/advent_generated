
#lang racket

(define (read-all path)
  (with-input-from-file path
    (lambda () (port->string (current-input-port)))))

(define (process-input lines)
  (define x (list 1))
  (for ([line lines])
    (cond
      [(string=? line "noop") (set! x (append x (list (last x))))]
      [else
       (define n (string->number (second (string-split line))))
       (set! x (append x (list (last x) (+ (last x) n))))]))
  x)

(define (create-grid x)
  (define grid (make-hash))
  (for ([i (in-range (length x))])
    (define crtx (modulo i 40))
    (define crty (quotient i 40))
    (when (<= (abs (- crtx (list-ref x i))) 1)
      (hash-set! grid (list crtx crty) #t)))
  grid)

(define (print-grid grid)
  (for ([y (in-range 6)])
    (for ([x (in-range 40)])
      (if (hash-ref grid (list x y) #f)
          (display "#")
          (display ".")))
    (newline)))

(define input (read-all "input.txt"))
(define lines (string-split input "\n"))
(define x (process-input lines))
(define grid (create-grid x))
(print-grid grid)
