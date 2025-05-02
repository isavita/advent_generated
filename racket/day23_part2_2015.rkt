
#lang racket

(define (main)
  (define instructions
    (with-input-from-file "input.txt"
      (lambda () (port->lines))))

  (define parsed-instructions
    (map string-split instructions))

  (define registers (make-hash))
  (hash-set! registers 'a 1)
  (hash-set! registers 'b 0)

  (define (execute pc regs)
    (if (or (< pc 0) (>= pc (length parsed-instructions)))
        regs
        (let* ((instruction (list-ref parsed-instructions pc))
               (op (list-ref instruction 0)))
          (case op
            (("hlf")
             (let ((reg-name (string->symbol (list-ref instruction 1))))
               (hash-update! regs reg-name (lambda (val) (quotient val 2)))
               (execute (+ pc 1) regs)))
            (("tpl")
             (let ((reg-name (string->symbol (list-ref instruction 1))))
               (hash-update! regs reg-name (lambda (val) (* val 3)))
               (execute (+ pc 1) regs)))
            (("inc")
             (let ((reg-name (string->symbol (list-ref instruction 1))))
               (hash-update! regs reg-name add1)
               (execute (+ pc 1) regs)))
            (("jmp")
             (let ((offset (string->number (list-ref instruction 1))))
               (execute (+ pc offset) regs)))
            (("jie")
             (let ((reg-name (string->symbol (substring (list-ref instruction 1) 0 1)))
                   (offset (string->number (list-ref instruction 2))))
               (if (even? (hash-ref regs reg-name))
                   (execute (+ pc offset) regs)
                   (execute (+ pc 1) regs))))
            (("jio")
             (let ((reg-name (string->symbol (substring (list-ref instruction 1) 0 1)))
                   (offset (string->number (list-ref instruction 2))))
               (if (= (hash-ref regs reg-name) 1)
                   (execute (+ pc offset) regs)
                   (execute (+ pc 1) regs))))
            (else
             regs)))))

  (define final-registers (execute 0 registers))
  (printf "~a\n" (hash-ref final-registers 'b)))

(main)
