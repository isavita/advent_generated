
#lang racket

(define undefined (cons 0 0))
(define top (cons 0 -1))
(define right (cons 1 0))
(define bottom (cons 0 1))
(define left (cons -1 0))

(define empty-tile ".")
(define start-tile "S")
(define vertical-tile "|")
(define horizontal-tile "-")
(define top-left-corner-tile "J")
(define top-right-corner-tile "L")
(define bottom-left-corner-tile "7")
(define bottom-right-corner-tile "F")

(define vertical-pipe (list top bottom))
(define horizontal-pipe (list left right))
(define top-left-corner-pipe (list top left))
(define top-right-corner-pipe (list top right))
(define bottom-left-corner-pipe (list bottom left))
(define bottom-right-corner-pipe (list bottom right))

(define tile->pipe (hash
                    vertical-tile vertical-pipe
                    horizontal-tile horizontal-pipe
                    top-left-corner-tile top-left-corner-pipe
                    top-right-corner-tile top-right-corner-pipe
                    bottom-left-corner-tile bottom-left-corner-pipe
                    bottom-right-corner-tile bottom-right-corner-pipe))

(define (add-coords p1 p2)
  (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))

(define (negate-coord p)
  (cons (* (car p) -1) (* (cdr p) -1)))

(define (build-grid input)
  (let ([grid (make-hash)])
    (for ([line (in-list input)]
          [y (in-naturals)])
      (for ([char (in-string line)]
            [x (in-naturals)])
        (unless (string=? (string char) empty-tile)
          (hash-set! grid (cons x y) (string char)))))
    grid))

(define (find-start grid)
  (for/first ([(coord tile) (in-hash grid)] #:when (string=? tile start-tile))
    coord))

(define (get-pipe-from-tile tile)
  (hash-ref tile->pipe tile '()))

(define (get-start-pipe start-coord grid)
  (let ([possible-neighbors (list top right bottom left)]
        [start-pipe '()])
    (for ([dir possible-neighbors])
      (let* ([neighbor-coord (add-coords start-coord dir)]
             [neighbor-tile (hash-ref grid neighbor-coord #f)])
        (when neighbor-tile
          (let ([neighbor-pipe (get-pipe-from-tile neighbor-tile)])
            (when (member (negate-coord dir) neighbor-pipe equal?)
              (set! start-pipe (cons dir start-pipe)))))))
    start-pipe))

(define (find-next-dir pipe prev-dir)
  (let ([prev-dir-negated (negate-coord prev-dir)])
    (findf (lambda (dir) (not (equal? dir prev-dir-negated))) pipe)))

(define (pathfinding start-coord grid)
  (let* ([start-pipe (get-start-pipe start-coord grid)]
         [first-dir (first start-pipe)]
         [path (list start-coord)]
         [current-coord (add-coords start-coord first-dir)]
         [previous-dir first-dir])
    (let loop ([current current-coord]
               [prev-dir previous-dir]
               [current-path path])
      (if (equal? current start-coord)
          (reverse current-path)
          (let* ([current-tile (hash-ref grid current)]
                 [current-pipe (get-pipe-from-tile current-tile)]
                 [next-dir (find-next-dir current-pipe prev-dir)]
                 [next-coord (add-coords current next-dir)])
            (loop next-coord next-dir (cons current current-path)))))))

(define (solve input)
  (let* ([grid (build-grid input)]
         [start-coord (find-start grid)]
         [loop-path (pathfinding start-coord grid)])
    (floor (/ (length loop-path) 2))))

(define (read-file filename)
  (with-input-from-file filename
    (lambda () (port->lines (current-input-port)))))

(define (main)
  (let ([input (read-file "input.txt")])
    (display (solve input))
    (newline)))

(main)
