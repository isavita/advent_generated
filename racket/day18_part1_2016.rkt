
#lang racket

(define (generate-next-row prev-row)
  (define len (length prev-row))
  (define next-row
    (for/list ([i (in-range len)])
      (define left (if (= i 0) #\. (list-ref prev-row (- i 1))))
      (define center (list-ref prev-row i))
      (define right (if (= i (- len 1)) #\. (list-ref prev-row (+ i 1))))
      (cond
        ((and (equal? left #\^) (equal? center #\^) (equal? right #\.)) #\^)
        ((and (equal? left #\.) (equal? center #\^) (equal? right #\^)) #\^)
        ((and (equal? left #\^) (equal? center #\.) (equal? right #\.)) #\^)
        ((and (equal? left #\.) (equal? center #\.) (equal? right #\^)) #\^)
        (else #\.))))
  next-row)

(define (count-safe-tiles rows)
  (apply + (map (lambda (row) (count (lambda (tile) (equal? tile #\.)) row)) rows)))

(define (solve input-str num-rows)
  (define initial-row (string->list input-str))
  (define rows (list initial-row))
  (for ([i (in-range (- num-rows 1))])
    (set! rows (append rows (list (generate-next-row (last rows))))))
  (count-safe-tiles rows))

(define (main)
  (define input-str (with-input-from-file "input.txt" read-line))
  (displayln (solve input-str 40)))

(main)
