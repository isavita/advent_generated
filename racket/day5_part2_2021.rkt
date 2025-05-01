
#lang racket

(require racket/string)
(require racket/match)

(define (parse-line line)
  (match (string-split line " -> ")
    [(list start-str end-str)
     (match (string-split start-str ",")
       [(list x1-str y1-str)
        (match (string-split end-str ",")
          [(list x2-str y2-str)
           (list (string->number x1-str)
                 (string->number y1-str)
                 (string->number x2-str)
                 (string->number y2-str))])])]))

(define (sign x)
  (cond
    [(> x 0) 1]
    [(< x 0) -1]
    [else 0]))

(define (main)
  (define lines
    (with-input-from-file "input.txt"
      (lambda ()
        (define input-lines (port->lines (current-input-port)))
        (map parse-line input-lines))))

  (define overlaps (make-hash))

  (for ([line lines])
    (match line
      [(list x1 y1 x2 y2)
       (define x-step (sign (- x2 x1)))
       (define y-step (sign (- y2 y1)))
       (define steps (+ (max (abs (- x2 x1)) (abs (- y2 y1))) 1))

       (for ([i (in-range steps)])
         (define current-x (+ x1 (* i x-step)))
         (define current-y (+ y1 (* i y-step)))
         (define point (cons current-x current-y))

         (hash-set! overlaps point (+ (hash-ref overlaps point 0) 1)))]))

  (define count
    (for/sum ([v (hash-values overlaps)])
      (if (> v 1) 1 0)))

  (print count))

(module+ main
  (main))
