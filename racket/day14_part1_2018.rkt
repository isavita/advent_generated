
#lang racket

(require racket/vector)
(require racket/port)

(define (main)
  (let ([input-file "input.txt"])
    (with-input-from-file input-file
      (lambda ()
        (let* ([input (read)]
               [target-len (+ input 10)]
               [max-len (+ input 11)]
               [scoreboard (make-vector max-len 0)]
               [current-len 2]
               [elf1 0]
               [elf2 1])
          (vector-set! scoreboard 0 3)
          (vector-set! scoreboard 1 7)
          (let recipe-loop ()
            (if (< current-len target-len)
                (let* ([score1 (vector-ref scoreboard elf1)]
                       [score2 (vector-ref scoreboard elf2)]
                       [new-score (+ score1 score2)])
                  (when (>= new-score 10)
                    (vector-set! scoreboard current-len (quotient new-score 10))
                    (set! current-len (add1 current-len)))
                  (vector-set! scoreboard current-len (modulo new-score 10))
                  (set! current-len (add1 current-len))
                  (set! elf1 (modulo (+ elf1 score1 1) current-len))
                  (set! elf2 (modulo (+ elf2 score2 1) current-len))
                  (recipe-loop))
                (void)))
          (for ([i (in-range input target-len)])
            (display (vector-ref scoreboard i)))
          (newline))))))

(main)
