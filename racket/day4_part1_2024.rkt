
#lang racket

(require racket/file)
(require racket/string)

(define (main)
  (define grid (file->lines "input.txt"))
  (define r (length grid))
  (define c (if (> r 0) (string-length (first grid)) 0))

  (define word "XMAS")
  (define L (string-length word))
  (define dirs '((1 0) (-1 0) (0 1) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1)))

  (define count 0)

  (for* ([i (in-range r)]
         [j (in-range c)])
    (when (char=? (string-ref (list-ref grid i) j) (string-ref word 0))
      (for ([dir dirs])
        (define dx (first dir))
        (define dy (second dir))

        (let loop ([x i] [y j] [k 0])
          (if (and (>= x 0) (< x r) (>= y 0) (< y c) (< k L)
                   (char=? (string-ref (list-ref grid x) y) (string-ref word k)))
              (loop (+ x dx) (+ y dy) (+ k 1))
              (when (= k L)
                (set! count (+ count 1))))))))

  (printf "~a~%" count))

(main)
