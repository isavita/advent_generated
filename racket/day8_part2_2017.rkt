
#lang racket

(define (solve)
  (define registers (make-hash))
  (define highest-value 0)

  (define (get-register reg)
    (hash-ref registers reg 0))

  (define (set-register! reg val)
    (hash-set! registers reg val)
    (when (> val highest-value)
      (set! highest-value val)))

  (define (process-instruction line)
    (match (string-split line)
      [(list reg op amount _ cond-reg cond-op cond-val)
       (define amount-val (string->number amount))
       (define cond-val-val (string->number cond-val))
       (define cond-result
         (cond
           [(string=? cond-op ">") (> (get-register cond-reg) cond-val-val)]
           [(string=? cond-op ">=") (>= (get-register cond-reg) cond-val-val)]
           [(string=? cond-op "<") (< (get-register cond-reg) cond-val-val)]
           [(string=? cond-op "<=") (<= (get-register cond-reg) cond-val-val)]
           [(string=? cond-op "==") (= (get-register cond-reg) cond-val-val)]
           [(string=? cond-op "!=") (not (= (get-register cond-reg) cond-val-val))]))
       (when cond-result
         (cond
           [(string=? op "inc") (set-register! reg (+ (get-register reg) amount-val))]
           [(string=? op "dec") (set-register! reg (- (get-register reg) amount-val))]))]))

  (call-with-input-file "input.txt"
    (lambda (in)
      (for ([line (in-lines in)])
        (process-instruction line))))

  (printf "~a\n" highest-value))

(solve)
