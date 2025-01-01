
#lang racket

(define (some-assembly-required input)
  (define wire-to-rule (make-hash))
  (for ([line (string-split input "\n")])
    (define parts (string-split line " -> "))
    (hash-set! wire-to-rule (second parts) (first parts)))

  (define (memo-dfs graph entry memo)
    (cond
      [(hash-has-key? memo entry) (hash-ref memo entry)]
      [(regexp-match? #rx"^[0-9]+$" entry) (string->number entry)]
      [else
       (define source-rule (hash-ref graph entry))
       (define parts (string-split source-rule " "))
       (define result
         (cond
           [(= (length parts) 1) (memo-dfs graph (first parts) memo)]
           [(string=? (first parts) "NOT") (bitwise-not (memo-dfs graph (second parts) memo))]
           [(string=? (second parts) "AND") (bitwise-and (memo-dfs graph (first parts) memo) (memo-dfs graph (third parts) memo))]
           [(string=? (second parts) "OR") (bitwise-ior (memo-dfs graph (first parts) memo) (memo-dfs graph (third parts) memo))]
           [(string=? (second parts) "LSHIFT") (arithmetic-shift (memo-dfs graph (first parts) memo) (memo-dfs graph (third parts) memo))]
           [(string=? (second parts) "RSHIFT") (arithmetic-shift (memo-dfs graph (first parts) memo) (- (memo-dfs graph (third parts) memo)))]
           [else (error "Invalid rule")]))
       (hash-set! memo entry (bitwise-and result #xFFFF))
       (bitwise-and result #xFFFF)]))

  (define a-signal (memo-dfs wire-to-rule "a" (make-hash)))
  (hash-set! wire-to-rule "b" (number->string a-signal))
  (memo-dfs wire-to-rule "a" (make-hash)))

(define input (file->string "input.txt"))
(printf "~a\n" (some-assembly-required input))
