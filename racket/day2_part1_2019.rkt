#lang racket

(define (run-intcode-program lst)
  (define (process-opcode lst idx)
    (define op (list-ref lst idx))
    (cond
      ((= op 99) lst)
      ((= op 1)
       (let* ((pos1 (list-ref lst (+ idx 1)))
              (pos2 (list-ref lst (+ idx 2)))
              (pos3 (list-ref lst (+ idx 3)))
              (val1 (list-ref lst pos1))
              (val2 (list-ref lst pos2))
              (new-lst (list-set lst pos3 (+ val1 val2))))
         (process-opcode new-lst (+ idx 4))))
      ((= op 2)
       (let* ((pos1 (list-ref lst (+ idx 1)))
              (pos2 (list-ref lst (+ idx 2)))
              (pos3 (list-ref lst (+ idx 3)))
              (val1 (list-ref lst pos1))
              (val2 (list-ref lst pos2))
              (new-lst (list-set lst pos3 (* val1 val2))))
         (process-opcode new-lst (+ idx 4))))
      (else (error "Unknown opcode" op))))
  
  (process-opcode lst 0))

(define input (file->lines "input.txt"))
(define program (map string->number (string-split (car input) ",")))

(define (replace-at lst idx val)
  (cond
    ((null? lst) '())
    ((= idx 0) (cons val (cdr lst)))
    (else (cons (car lst) (replace-at (cdr lst) (- idx 1) val)))))

(define modified-program (replace-at (replace-at program 1 12) 2 2))

(define result-program (run-intcode-program modified-program))

(printf "Value at position 0 after program halts: ~a\n" (list-ref result-program 0))