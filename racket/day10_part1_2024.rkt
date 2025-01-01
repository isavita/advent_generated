
#lang racket

(define (solve)
  (define lines (file->lines "input.txt"))
  (define nr (length lines))
  (define nc (string-length (first lines)))
  (define grid (make-vector nr))

  (for ([i nr])
    (define line (list->string (string->list (list-ref lines i))))
    (vector-set! grid i (make-vector nc))
    (for ([j nc])
      (vector-set! (vector-ref grid i) j (- (char->integer (string-ref line j)) (char->integer #\0)))))

  (define dirs (list (cons 1 0) (cons -1 0) (cons 0 1) (cons 0 -1)))
  (define trailheads '())

  (for ([r nr])
    (for ([c nc])
      (when (= (vector-ref (vector-ref grid r) c) 0)
        (set! trailheads (cons (cons r c) trailheads)))))

  (define sum-scores 0)
  (for ([th trailheads])
    (define reached (make-hash))
    (define front (list (list th 0)))
    (define visited (make-hash))

    (let loop ([front front])
      (unless (null? front)
        (define cur (car front))
        (define rest (cdr front))
        (define p (first cur))
        (define h (second cur))

        (cond
          [(= h 9)
           (unless (hash-has-key? reached p)
             (hash-set! reached p #t))]
          [else
           (for ([d dirs])
             (define nr2 (+ (car p) (car d)))
             (define nc2 (+ (cdr p) (cdr d)))
             (when (and (>= nr2 0) (< nr2 nr) (>= nc2 0) (< nc2 nc)
                        (= (vector-ref (vector-ref grid nr2) nc2) (+ h 1)))
               (define key (list nr2 nc2 (+ h 1)))
               (unless (hash-has-key? visited key)
                 (hash-set! visited key #t)
                 (set! rest (append rest (list (list (cons nr2 nc2) (+ h 1))))))))])
        (loop rest)))
    (set! sum-scores (+ sum-scores (hash-count reached))))

  (printf "~a\n" sum-scores))

(solve)
