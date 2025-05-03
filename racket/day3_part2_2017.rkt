
#lang racket

(require racket/port)
(require racket/hash)

(define (main)
  (define target
    (with-input-from-file "input.txt"
      (lambda ()
        (string->number (string-trim (read-line))))))

  (define grid (make-hash))
  (hash-set! grid (cons 0 0) 1)

  (define x 0)
  (define y 0)
  (define dx 0)
  (define dy -1)

  (let loop ()
    (when (or (= x y)
              (and (< x 0) (= x (- y)))
              (and (> x 0) (= x (- 1 y))))
      (let ((old-dx dx))
        (set! dx (- dy))
        (set! dy old-dx)))

    (set! x (+ x dx))
    (set! y (+ y dy))

    (define value 0)
    (for* ((i (range -1 2))
           (j (range -1 2)))
      (set! value (+ value (hash-ref grid (cons (+ x i) (+ y j)) 0))))

    (hash-set! grid (cons x y) value)

    (when (> value target)
      (displayln value)
      (exit))

    (loop)))

(main)
