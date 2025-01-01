
#lang racket

(define (solve)
  (define in (open-input-file "input.txt"))
  (define left '())
  (define right '())
  (let loop ([line (read-line in 'any)])
    (cond
      [(eof-object? line)
       (close-input-port in)
       (unless (= (length left) (length right))
         (error "Lists have different lengths"))
       (set! left (sort left <))
       (set! right (sort right <))
       (apply + (map (lambda (x y) (abs (- x y))) left right))]
      [else
       (let* ([fields (string-split line)]
              [left-num (string->number (list-ref fields 0))]
              [right-num (string->number (list-ref fields 1))])
         (unless (and (number? left-num) (number? right-num))
           (error "Invalid number in input"))
         (set! left (append left (list left-num)))
         (set! right (append right (list right-num)))
         (loop (read-line in 'any)))])))

(printf "~a\n" (solve))
