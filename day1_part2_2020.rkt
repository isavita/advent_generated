
#lang racket

(define (find-two-entries-that-sum-to-2020 lst)
  (for*/first ([x lst] [y lst] #:when (= (+ x y) 2020))
    (* x y)))

(define (find-three-entries-that-sum-to-2020 lst)
  (for*/first ([x lst] [y lst] [z lst] #:when (= (+ x y z) 2020))
    (* x y z)))

(define input (file->lines "input.txt"))
(define expenses (map string->number input))

(printf "Part One: ~a\n" (find-two-entries-that-sum-to-2020 expenses))
(printf "Part Two: ~a\n" (find-three-entries-that-sum-to-2020 expenses))
