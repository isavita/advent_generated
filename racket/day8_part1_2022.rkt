
#lang racket

(define (read-grid filename)
  (define grid (make-hash))
  (define (process-line y line)
    (for ([x (in-range (string-length line))])
      (hash-set! grid (list x y) (- (char->integer (string-ref line x)) (char->integer #\0)))))
  (define lines (file->lines filename))
  (for ([y (in-range (length lines))])
    (process-line y (list-ref lines y)))
  grid)

(define (visible-count grid)
  (define directions '((0 1) (0 -1) (1 0) (-1 0)))
  (define visible (make-hash))
  (for ([p (hash-keys grid)])
    (for ([d directions])
      (define next p)
      (define (next-point pt dir)
        (list (+ (first pt) (first dir)) (+ (second pt) (second dir))))
      (let loop ()
        (set! next (next-point next d))
        (cond
          [(hash-has-key? grid next)
           (if (>= (hash-ref grid next) (hash-ref grid p))
               (void)
               (loop))]
          [else (hash-set! visible p #t)]))))
  (hash-count visible))

(define (main)
  (define grid (read-grid "input.txt"))
  (printf "~a\n" (visible-count grid)))

(main)
