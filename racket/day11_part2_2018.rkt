
#lang racket

(define (calculate-power-level x y serial-number)
  (let* ([rack-id (+ x 10)]
         [power-level (* rack-id y)]
         [power-level (+ power-level serial-number)]
         [power-level (* power-level rack-id)]
         [power-level (quotient power-level 100)]
         [power-level (remainder power-level 10)]
         [power-level (- power-level 5)])
    power-level))

(define (make-grid width height initial-value)
  (build-vector height (lambda (i) (build-vector width (lambda (j) initial-value)))))

(define (calculate-cumulative-grid grid)
  (let* ([size 300]
         [cumulative-grid (make-grid (+ size 1) (+ size 1) 0)])
    (for ([x (in-range 1 (+ size 1))])
      (for ([y (in-range 1 (+ size 1))])
        (let ([grid-val (vector-ref (vector-ref grid (sub1 x)) (sub1 y))]
              [cum-up (vector-ref (vector-ref cumulative-grid (sub1 x)) y)]
              [cum-left (vector-ref (vector-ref cumulative-grid x) (sub1 y))]
              [cum-upleft (vector-ref (vector-ref cumulative-grid (sub1 x)) (sub1 y))])
          (vector-set! (vector-ref cumulative-grid x) y
                       (+ grid-val cum-up cum-left (- cum-upleft))))))
    cumulative-grid))

(define (calculate-total-power cumulative-grid x y size)
  (let* ([x1 (sub1 x)]
         [y1 (sub1 y)]
         [x2 (+ x size -1)]
         [y2 (+ y size -1)])
    (let ([cum-br (vector-ref (vector-ref cumulative-grid x2) y2)]
          [cum-bl (vector-ref (vector-ref cumulative-grid x1) y2)]
          [cum-tr (vector-ref (vector-ref cumulative-grid x2) y1)]
          [cum-tl (vector-ref (vector-ref cumulative-grid x1) y1)])
      (+ cum-br (- cum-bl) (- cum-tr) cum-tl))))

(module+ main
  (define serial-number
    (with-input-from-file "input.txt"
      (lambda ()
        (string->number (string-trim (read-line))))))

  (define grid (make-grid 300 300 0))
  (for ([x (in-range 0 300)])
    (for ([y (in-range 0 300)])
      (vector-set! (vector-ref grid x) y
                   (calculate-power-level (add1 x) (add1 y) serial-number))))

  (define cumulative-grid (calculate-cumulative-grid grid))

  (define max-power -inf.0)
  (define max-coords '())

  (for ([size (in-range 1 301)])
    (for ([x (in-range 1 (- 301 size))])
      (for ([y (in-range 1 (- 301 size))])
        (let ([total-power (calculate-total-power cumulative-grid x y size)])
          (when (> total-power max-power)
            (set! max-power total-power)
            (set! max-coords (list x y size)))))))

  (printf "~a,~a,~a\n" (first max-coords) (second max-coords) (third max-coords)))
