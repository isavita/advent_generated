
#lang racket

(require racket/hash)
(require racket/file)
(require racket/string)

(define (get-val op regs)
  (if (number? op)
      op
      (hash-ref regs op 0)))

(define (run instructions)
  (define num-instructions (vector-length instructions))
  (define regs (make-hash))
  (define ip (box 0))
  (define last-snd (box 0))

  (let loop ()
    (define current-ip (unbox ip))

    (when (and (>= current-ip 0) (< current-ip num-instructions))
      (define instruction (vector-ref instructions current-ip))
      (define command (vector-ref instruction 0))
      (define op1 (vector-ref instruction 1))

      (define next-ip (add1 current-ip))

      (case command
        [(snd)
         (define val (get-val op1 regs))
         (set-box! last-snd val)]
        [(set)
         (define op2 (vector-ref instruction 2))
         (define val (get-val op2 regs))
         (hash-set! regs op1 val)]
        [(add)
         (define op2 (vector-ref instruction 2))
         (define val (get-val op2 regs))
         (hash-update! regs op1 (lambda (v) (+ v val)) 0)]
        [(mul)
         (define op2 (vector-ref instruction 2))
         (define val (get-val op2 regs))
         (hash-update! regs op1 (lambda (v) (* v val)) 0)]
        [(mod)
         (define op2 (vector-ref instruction 2))
         (define val (get-val op2 regs))
         (define cur-val (get-val op1 regs))
         (hash-set! regs op1 (modulo cur-val val))]
        [(rcv)
         (define val (get-val op1 regs))
         (when (not (zero? val))
           (displayln (unbox last-snd))
           (exit))]
        [(jgz)
         (define op2 (vector-ref instruction 2))
         (define val1 (get-val op1 regs))
         (define val2 (get-val op2 regs))
         (when (> val1 0)
           (set! next-ip (+ current-ip val2)))]
        [else
         (error 'run-program "Unknown instruction" command)])

      (set-box! ip next-ip)
      (loop))))

(define (main)
  (define raw-instructions (file->lines "input.txt"))
  (define parsed-instructions
    (for/vector ([line raw-instructions])
      (for/vector ([part (string-split line)])
        (let ([num (string->number part)])
          (if num num (string->symbol part))))))

  (run parsed-instructions))

(main)
