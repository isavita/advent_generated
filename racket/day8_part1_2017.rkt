
#lang racket

(define (parse-condition condition)
  (let ([parts (string-split condition)])
    (list (string->symbol (first parts))
          (second parts)
          (string->number (third parts)))))

(define (evaluate-condition registers reg op value)
  (define reg-value (hash-ref registers reg 0))
  (cond
    [(equal? op ">") (> reg-value value)]
    [(equal? op "<") (< reg-value value)]
    [(equal? op ">=") (>= reg-value value)]
    [(equal? op "<=") (<= reg-value value)]
    [(equal? op "==") (= reg-value value)]
    [(equal? op "!=") (not (= reg-value value))]))

(define (process-instruction registers instruction)
  (define parts (string-split instruction))
  (define reg (string->symbol (first parts)))
  (define action (second parts))
  (define amount (string->number (third parts)))
  (define condition (parse-condition (string-join (drop parts 4) " ")))

  (when (evaluate-condition registers (first condition) (second condition) (third condition))
    (define new-value
      (if (equal? action "inc")
          (+ (hash-ref registers reg 0) amount)
          (- (hash-ref registers reg 0) amount)))
    (hash-set! registers reg new-value)))

(define (process-instructions filename)
  (define registers (make-hash))
  (define instructions (file->lines filename))

  (for-each (lambda (instruction)
              (process-instruction registers instruction))
            instructions)

  (apply max (hash-values registers)))

(define (main)
  (define result (process-instructions "input.txt"))
  (printf "The largest value in any register is: ~a\n" result))

(main)
