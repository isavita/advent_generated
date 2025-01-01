
#lang racket

(define (get-value s regs)
  (define val (string->number s))
  (if val
      val
      (hash-ref regs s)))

(define (execute-instructions instructions regs)
  (let loop ([i 0])
    (cond
      [(= i (length instructions)) regs]
      [else
       (define parts (string-split (list-ref instructions i)))
       (case (first parts)
         [("cpy")
          (define val (get-value (second parts) regs))
          (hash-set! regs (third parts) val)
          (loop (+ i 1))]
         [("inc")
          (hash-set! regs (second parts) (+ (hash-ref regs (second parts)) 1))
          (loop (+ i 1))]
         [("dec")
          (hash-set! regs (second parts) (- (hash-ref regs (second parts)) 1))
          (loop (+ i 1))]
         [("jnz")
          (define val (get-value (second parts) regs))
          (if (not (= val 0))
              (loop (+ i (string->number (third parts))))
              (loop (+ i 1)))]
         [else (error "Invalid instruction")])])))

(define (main)
  (define instructions
    (call-with-input-file "input.txt"
      (lambda (in)
        (for/list ([line (in-lines in)])
          line))))
  (define regs (make-hash))
  (hash-set! regs "a" 0)
  (hash-set! regs "b" 0)
  (hash-set! regs "c" 1)
  (hash-set! regs "d" 0)
  (define result-regs (execute-instructions instructions regs))
  (displayln (hash-ref result-regs "a")))

(main)
