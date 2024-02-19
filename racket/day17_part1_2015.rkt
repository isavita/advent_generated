
#lang racket

(define (count-combinations containers target index)
  (cond
    ((= target 0) 1)
    ((or (< target 0) (>= index (length containers))) 0)
    (else (+ (count-combinations containers (- target (list-ref containers index)) (+ index 1))
             (count-combinations containers target (+ index 1))))))

(define containers (file->list "input.txt"))

(printf "~a\n" (count-combinations containers 150 0))
