
#lang racket

(require racket/string)
(require racket/file)

(define (read-grid filename)
  (map (lambda (line)
         (map (lambda (char) (- (char->integer char) (char->integer #\0)))
              (string->list line)))
       (file->lines filename)))

(define (solve grid)
  (let* ((height (length grid))
         (width (if (empty? grid) 0 (length (first grid))))
         (total-risk 0))
    (for* ((y (in-range height))
           (x (in-range width)))
      (let ((h (list-ref (list-ref grid y) x))
            (is-low-point #t))
        (when (and (> x 0) (<= (list-ref (list-ref grid y) (- x 1)) h))
          (set! is-low-point #f))
        (when (and is-low-point (< x (- width 1)) (<= (list-ref (list-ref grid y) (+ x 1)) h))
          (set! is-low-point #f))
        (when (and is-low-point (> y 0) (<= (list-ref (list-ref grid (- y 1)) x) h))
          (set! is-low-point #f))
        (when (and is-low-point (< y (- height 1)) (<= (list-ref (list-ref grid (+ y 1)) x) h))
          (set! is-low-point #f))
        (when is-low-point
          (set! total-risk (+ total-risk 1 h)))))
    total-risk))

(define (main)
  (let ((grid (read-grid "input.txt")))
    (println (solve grid))))

(module* main #f (main))
