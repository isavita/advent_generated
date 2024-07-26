
#lang racket

(define (calculate-score opponent round-end)
  (define your-move
    (cond
      [(equal? round-end #\X) (case opponent
                                [(#\A) #\Z]
                                [(#\B) #\X]
                                [(#\C) #\Y])]
      [(equal? round-end #\Y) (case opponent
                                [(#\A) #\X]
                                [(#\B) #\Y]
                                [(#\C) #\Z])]
      [else (case opponent
              [(#\A) #\Y]
              [(#\B) #\Z]
              [(#\C) #\X])]))

  (define score (case your-move
                  [(#\X) 1]
                  [(#\Y) 2]
                  [(#\Z) 3]))

  (define win-conditions
    (or (and (equal? opponent #\A) (equal? your-move #\Y))
        (and (equal? opponent #\B) (equal? your-move #\Z))
        (and (equal? opponent #\C) (equal? your-move #\X))))

  (define draw-conditions
    (or (and (equal? opponent #\A) (equal? your-move #\X))
        (and (equal? opponent #\B) (equal? your-move #\Y))
        (and (equal? opponent #\C) (equal? your-move #\Z))))

  (+ score (if win-conditions 6 (if draw-conditions 3 0))))

(define (process-file filename)
  (define in (open-input-file filename))
  (define total-score 0)

  (for ([line (in-lines in)])
    (define opponent (string-ref line 0))
    (define round-end (string-ref line 2))
    (set! total-score (+ total-score (calculate-score opponent round-end))))

  (close-input-port in)
  total-score)

(define result (process-file "input.txt"))
(displayln result)
