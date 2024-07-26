
#lang racket

(define (parse-instruction line)
  (match (string-split line)
    [(list op arg) (list op (string->number arg))]))

(define (execute-instructions instructions)
  (define accumulator 0)
  (define visited (make-hash))
  (define index 0)

  (define (run)
    (if (hash-ref visited index #f)
        accumulator  ; If we've visited this index before, return the accumulator
        (begin
          (hash-set! visited index #t)  ; Mark this index as visited
          (match (list-ref instructions index)
            [(list "acc" n) 
             (set! accumulator (+ accumulator n))
             (set! index (+ index 1))]
            [(list "jmp" n) 
             (set! index (+ index n))]
            [(list "nop" _)
             (set! index (+ index 1))]
            [else (error "Unknown instruction")])
          (run))))  ; Continue running

  (run))

(define (main)
  (define input (file->lines "input.txt"))
  (define instructions (map parse-instruction input))
  (define result (execute-instructions instructions))
  (printf "Accumulator value before any instruction is executed a second time: ~a\n" result))

(main)
