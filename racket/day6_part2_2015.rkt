
#lang racket

(define grid-size 1000)

(define (process-instruction instruction grid)
  (let* ([parts (string-split instruction)]
         [start (string-split (list-ref parts (- (length parts) 3)) ",")]
         [end (string-split (list-ref parts (- (length parts) 1)) ",")]
         [start-x (string->number (list-ref start 0))]
         [start-y (string->number (list-ref start 1))]
         [end-x (string->number (list-ref end 0))]
         [end-y (string->number (list-ref end 1))])
    (for ([x (in-range start-x (+ end-x 1))])
      (for ([y (in-range start-y (+ end-y 1))])
        (cond
          [(string-prefix? instruction "turn on")
           (vector-set! (vector-ref grid x) y (+ (vector-ref (vector-ref grid x) y) 1))]
          [(string-prefix? instruction "turn off")
           (when (> (vector-ref (vector-ref grid x) y) 0)
             (vector-set! (vector-ref grid x) y (- (vector-ref (vector-ref grid x) y) 1)))]
          [(string-prefix? instruction "toggle")
           (vector-set! (vector-ref grid x) y (+ (vector-ref (vector-ref grid x) y) 2))])))))

(define (total-brightness grid)
  (apply + (for/list ([row grid]) (apply + (vector->list row)))))

(define grid (build-vector grid-size (lambda (x) (make-vector grid-size 0))))

(call-with-input-file "input.txt"
  (lambda (in)
    (for ([line (in-lines in)])
      (process-instruction line grid))))

(displayln (total-brightness grid))
