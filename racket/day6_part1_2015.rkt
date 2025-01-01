
#lang racket

(define grid-size 1000)

(define (process-instruction instruction grid)
  (let* ([parts (string-split instruction)]
         [start (string-split (list-ref parts (- (length parts) 3)) ",")]
         [end (string-split (list-ref parts (- (length parts) 1)) ",")]
         [startX (string->number (list-ref start 0))]
         [startY (string->number (list-ref start 1))]
         [endX (string->number (list-ref end 0))]
         [endY (string->number (list-ref end 1))])
    (for ([x (in-range startX (+ endX 1))])
      (for ([y (in-range startY (+ endY 1))])
        (cond
          [(string-prefix? instruction "turn on") (vector-set! (vector-ref grid x) y #t)]
          [(string-prefix? instruction "turn off") (vector-set! (vector-ref grid x) y #f)]
          [(string-prefix? instruction "toggle") (vector-set! (vector-ref grid x) y (not (vector-ref (vector-ref grid x) y)))])))))

(define (count-lights grid)
  (let ([count 0])
    (for ([row grid])
      (for ([light row])
        (when light (set! count (+ count 1)))))
    count))

(define (main)
  (define grid (build-vector grid-size (λ (x) (make-vector grid-size #f))))
  (call-with-input-file "input.txt"
    (λ (in)
      (for ([line (in-lines in)])
        (process-instruction line grid))))
  (printf "~a\n" (count-lights grid)))

(main)
