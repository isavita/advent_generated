
#lang racket

(define (solve)
  (define lines (file->lines "input.txt"))
  (define nr (length lines))
  (define nc (string-length (list-ref lines 0)))
  (define grid (make-vector nr))
  (for ([i nr])
    (define row (make-vector nc))
    (for ([j nc])
      (vector-set! row j (- (char->integer (string-ref (list-ref lines i) j)) (char->integer #\0))))
    (vector-set! grid i row))

  (define dp (make-vector nr))
  (for ([i nr])
    (define row (make-vector nc -1))
    (vector-set! dp i row))

  (define dirs (list (cons 1 0) (cons -1 0) (cons 0 1) (cons 0 -1)))

  (define (dfs r c)
    (cond
      [(= (vector-ref (vector-ref dp r) c) -1)
       (define h (vector-ref (vector-ref grid r) c))
       (cond
         [(= h 9)
          (vector-set! (vector-ref dp r) c 1)
          1]
         [else
          (define sum
            (for/sum ([d dirs])
              (define nr2 (+ r (car d)))
              (define nc2 (+ c (cdr d)))
              (cond
                [(or (< nr2 0) (>= nr2 nr) (< nc2 0) (>= nc2 nc)) 0]
                [(= (vector-ref (vector-ref grid nr2) nc2) (+ h 1)) (dfs nr2 nc2)]
                [else 0])))
          (vector-set! (vector-ref dp r) c sum)
          sum])]
      [else (vector-ref (vector-ref dp r) c)]))

  (define total
    (for*/sum ([r nr] [c nc])
      (if (= (vector-ref (vector-ref grid r) c) 0)
          (dfs r c)
          0)))
  (printf "~a\n" total))

(solve)
