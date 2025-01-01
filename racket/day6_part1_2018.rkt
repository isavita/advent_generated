
#lang racket

(define (read-points filename)
  (define points (list))
  (define max-x 0)
  (define max-y 0)
  (call-with-input-file filename
    (lambda (in)
      (let loop ()
        (define line (read-line in))
        (unless (eof-object? line)
          (define coords (string-split line ", "))
          (define x (string->number (first coords)))
          (define y (string->number (second coords)))
          (set! max-x (max max-x x))
          (set! max-y (max max-y y))
          (set! points (append points (list (cons x y))))
          (loop)))
      (values points max-x max-y))))

(define (manhattan-distance p1 p2)
  (+ (abs (- (car p1) (car p2))) (abs (- (cdr p1) (cdr p2)))))

(define (solve points max-x max-y)
  (define areas (make-vector (length points) 0))
  (define infinite (make-vector (length points) #f))
  (for* ([i (in-range 0 (+ max-x 2))]
         [j (in-range 0 (+ max-y 2))])
    (define min-dist (+ max-x max-y))
    (define closest-point -1)
    (for ([k (in-range (length points))]
          [point (in-list points)])
      (define dist (manhattan-distance (cons i j) point))
      (cond
        [(= dist min-dist) (set! closest-point -1)]
        [(< dist min-dist)
         (set! min-dist dist)
         (set! closest-point k)]))
    (when (not (= closest-point -1))
      (vector-set! areas closest-point (+ (vector-ref areas closest-point) 1))
      (when (or (= i 0) (= j 0) (= i (+ max-x 1)) (= j (+ max-y 1)))
        (vector-set! infinite closest-point #t))))
  (define max-area 0)
  (for ([i (in-range (length areas))]
        [area (in-vector areas)]
        [inf (in-vector infinite)])
    (when (and (not inf) (> area max-area))
      (set! max-area area)))
  max-area)

(define-values (points max-x max-y) (read-points "input.txt"))
(printf "~a\n" (solve points max-x max-y))
