
#lang racket

(define (manhattan-distance p1 p2)
  (apply + (map abs (map - p1 p2))))

(define (connected? point1 point2)
  (<= (manhattan-distance point1 point2) 3))

(define (find-constellations points)
  (define visited (make-vector (length points) #f))
  
  (define (dfs index)
    (when (not (vector-ref visited index))
      (vector-set! visited index #t)
      (for ([i (in-range (length points))])
        (when (and (not (vector-ref visited i))
                   (connected? (list-ref points index) (list-ref points i)))
          (dfs i)))))
  
  (define constellation-count 0)
  
  (for ([i (in-range (length points))])
    (when (not (vector-ref visited i))
      (dfs i)
      (set! constellation-count (+ constellation-count 1))))
  
  constellation-count)

(define (main)
  (define input (file->lines "input.txt"))
  (define points (map (lambda (line)
                        (map string->number (string-split line ",")))
                      input))
  (define result (find-constellations points))
  (printf "Number of constellations: ~a\n" result))

(main)
