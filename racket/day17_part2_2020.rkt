
#lang racket

(define (neighbors coord)
  (define x (list-ref coord 0))
  (define y (list-ref coord 1))
  (define z (list-ref coord 2))
  (define w (list-ref coord 3))
  (let ([neighbor-list '()])
    (for* ([dw (range -1 2)]
           [dz (range -1 2)]
           [dy (range -1 2)]
           [dx (range -1 2)])
      (unless (and (= dx 0) (= dy 0) (= dz 0) (= dw 0))
        (set! neighbor-list (cons (list (+ x dx) (+ y dy) (+ z dz) (+ w dw)) neighbor-list))))
    neighbor-list))

(define (simulate-cycle active-cubes)
  (define neighbor-counts (make-hash))
  (define candidates (make-hash))

  (for ([(coord _) (in-hash active-cubes)])
    (hash-set! candidates coord #t)
    (for ([neighbor-coord (neighbors coord)])
      (hash-update! neighbor-counts neighbor-coord add1 0)
      (hash-set! candidates neighbor-coord #t)))

  (define next-active-cubes (make-hash))
  (for ([(coord _) (in-hash candidates)])
    (define count (hash-ref neighbor-counts coord 0))
    (define is-active (hash-has-key? active-cubes coord))

    (when (or (and is-active (or (= count 2) (= count 3)))
              (and (not is-active) (= count 3)))
      (hash-set! next-active-cubes coord #t)))

  next-active-cubes)

(define (main)
  (define initial-state-lines
    (with-input-from-file "input.txt"
      (lambda () (port->lines (current-input-port)))))

  (define active-cubes (make-hash))

  (for ([line initial-state-lines]
        [y (in-naturals)])
    (for ([char (string->list line)]
          [x (in-naturals)])
      (when (char=? char #\#)
        (hash-set! active-cubes (list x y 0 0) #t))))

  (define final-active-cubes
    (for/fold ([current-active active-cubes])
              ([cycle (range 6)])
      (simulate-cycle current-active)))

  (printf "~a~%" (hash-count final-active-cubes)))

(main)
