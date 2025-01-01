
#lang racket

(define (power-level x y serial)
  (let* ((rack-id (+ x 10))
         (level (* rack-id y))
         (level (+ level serial))
         (level (* level rack-id))
         (hundreds-digit (if (>= level 100) (quotient (remainder level 1000) 100) 0)))
    (- hundreds-digit 5)))

(define (calculate-square-power grid x y)
  (let loop ((dx 0) (dy 0) (total 0))
    (if (= dy 3)
        total
        (if (= dx 3)
            (loop 0 (+ dy 1) total)
            (loop (+ dx 1) dy (+ total (list-ref (list-ref grid (+ y dy)) (+ x dx))))))))

(define (solve serial)
  (let* ((grid (for/list ((y (in-range 1 301)))
                 (for/list ((x (in-range 1 301)))
                   (power-level x y serial))))
         (max-power (make-vector 3 -inf.0))
         (max-x 0)
         (max-y 0))
    (for ((y (in-range 0 298)))
      (for ((x (in-range 0 298)))
        (let ((current-power (calculate-square-power grid x y)))
          (when (> current-power (vector-ref max-power 2))
            (vector-set! max-power 0 (+ x 1))
            (vector-set! max-power 1 (+ y 1))
            (vector-set! max-power 2 current-power)))))
    (printf "~a,~a~n" (vector-ref max-power 0) (vector-ref max-power 1))))

(define (main)
  (let ((input-port (open-input-file "input.txt")))
    (let ((serial (string->number (read-line input-port))))
      (solve serial))
    (close-input-port input-port)))

(main)
