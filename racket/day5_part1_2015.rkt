
#lang racket

(define (count-vowels str)
  (length (filter (lambda (c) (member c '(#\a #\e #\i #\o #\u))) (string->list str))))

(define (has-double str)
  (for/or ([i (in-range (sub1 (string-length str)))])
    (equal? (string-ref str i) (string-ref str (add1 i)))))

(define (is-nice-line line)
  (and (>= (count-vowels line) 3)
       (not (regexp-match? #"(ab|cd|pq|xy)" line))
       (has-double line)))

(define (count-nice-lines lines)
  (length (filter is-nice-line lines)))

(define (main)
  (define input (file->string "input.txt"))
  (define lines (map string-trim (string-split input "\n")))
  (printf "~a\n" (count-nice-lines lines)))

(main)
