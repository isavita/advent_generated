
#lang racket

(define (read-first-row filename)
  (call-with-input-file filename
    (lambda (in)
      (read-line in))))

(define (safe? index row)
  (cond
    [(or (< index 0) (>= index (string-length row))) #\.]
    [else (string-ref row index)]))

(define (trap? left center right)
  (or (and (char=? left #\^) (char=? center #\^) (char=? right #\.))
      (and (char=? center #\^) (char=? right #\^) (char=? left #\.))
      (and (char=? left #\^) (char=? center #\.) (char=? right #\.))
      (and (char=? right #\^) (char=? center #\.) (char=? left #\.))))

(define (count-safe-tiles first-row total-rows)
  (let loop ([row first-row] [row-num 1] [safe-count (for/sum ([c (in-string first-row)]) (if (char=? c #\.) 1 0))])
    (if (= row-num total-rows)
        safe-count
        (let* ([next-row (list->string
                           (for/list ([i (in-range (string-length row))])
                             (if (trap? (safe? (- i 1) row) (safe? i row) (safe? (+ i 1) row))
                                 #\^
                                 #\.)))]
               [new-safe-count (+ safe-count (for/sum ([c (in-string next-row)]) (if (char=? c #\.) 1 0)))])
          (loop next-row (+ row-num 1) new-safe-count)))))

(define first-row (read-first-row "input.txt"))
(displayln (count-safe-tiles first-row 400000))
