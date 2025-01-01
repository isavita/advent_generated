
#lang racket

(define (some-assembly-required input)
  (define wire-to-rule (make-hash))
  (for ([line (string-split input "\n")])
    (define parts (string-split line " -> "))
    (hash-set! wire-to-rule (second parts) (first parts)))

  (define memo (make-hash))
  (define (memo-dfs entry)
    (cond
      [(hash-has-key? memo entry) (hash-ref memo entry)]
      [(regexp-match? #rx"^[0-9]+$" entry) (string->number entry)]
      [else
       (define source-rule (hash-ref wire-to-rule entry))
       (define parts (string-split source-rule " "))
       (define result
         (cond
           [(= (length parts) 1) (memo-dfs (first parts))]
           [(string=? (first parts) "NOT") (bitwise-not (memo-dfs (second parts)))]
           [(string=? (second parts) "AND") (bitwise-and (memo-dfs (first parts)) (memo-dfs (third parts)))]
           [(string=? (second parts) "OR") (bitwise-ior (memo-dfs (first parts)) (memo-dfs (third parts)))]
           [(string=? (second parts) "LSHIFT") (arithmetic-shift (memo-dfs (first parts)) (memo-dfs (third parts)))]
           [(string=? (second parts) "RSHIFT") (arithmetic-shift (memo-dfs (first parts)) (- (memo-dfs (third parts))))]))
       (hash-set! memo entry (bitwise-and result #xFFFF))
       (hash-ref memo entry)]))
  (memo-dfs "a"))

(define input (file->string "input.txt"))
(printf "~a\n" (some-assembly-required input))
