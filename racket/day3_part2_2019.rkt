
#lang racket

(require racket/string)
(require racket/math)

(define (process-wire path-str)
  (define points (make-hash))
  (define current-pos (list 0 0))
  (define steps 0)

  (define moves (string-split path-str ","))

  (for ([move-str moves])
    (define direction (string-ref move-str 0))
    (define distance (string->number (substring move-str 1)))

    (for ([i (in-range distance)])
      (set! steps (add1 steps))

      (case direction
        [(#\U) (set! current-pos (list (first current-pos) (add1 (second current-pos))))]
        [(#\D) (set! current-pos (list (first current-pos) (sub1 (second current-pos))))]
        [(#\L) (set! current-pos (list (sub1 (first current-pos)) (second current-pos)))]
        [(#\R) (set! current-pos (list (add1 (first current-pos)) (second current-pos)))]
        [else (void)])

      (unless (hash-has-key? points current-pos)
        (hash-set! points current-pos steps))))
  points)

(define (solve)
  (define lines
    (with-input-from-file "input.txt"
      (lambda ()
        (list (read-line) (read-line)))))

  (define wire1-points (process-wire (first lines)))
  (define wire2-points (process-wire (second lines)))

  (define min-total-steps +inf.0)

  (for ([(point steps1) (in-hash wire1-points)])
    (when (hash-has-key? wire2-points point)
      (define steps2 (hash-ref wire2-points point))
      (set! min-total-steps (min min-total-steps (+ steps1 steps2)))))

  (print min-total-steps))

(solve)
