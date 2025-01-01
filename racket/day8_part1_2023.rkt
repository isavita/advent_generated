
#lang racket

(define (solve)
  (define input (file->string "input.txt"))
  (define lines (string-split input "\n"))
  (define instructions (string-trim (first lines)))
  (define desert-map (make-hash))

  (for ([line (cdr (cdr lines))] #:when (not (equal? line "")))
    (let* ([matches (regexp-match* #px"[A-Z]{3}" line)]
           [key (first matches)]
           [left (second matches)]
           [right (third matches)])
      (hash-set! desert-map key (cons left right))))

  (let loop ([current "AAA"] [steps 0] [instr-index 0])
    (cond
      [(equal? current "ZZZ") steps]
      [else
       (define direction (string-ref instructions instr-index))
       (define next-node (if (char=? direction #\R)
                            (cdr (hash-ref desert-map current))
                            (car (hash-ref desert-map current))))
       (loop next-node
             (add1 steps)
             (remainder (add1 instr-index) (string-length instructions)))])))

(printf "~a\n" (solve))
