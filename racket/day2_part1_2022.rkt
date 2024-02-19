
#lang racket

(define totalScore (open-input-file "input.txt"))

(define score 0)

(define (calculate-score opponent yourMove)
  (cond
    ((char=? yourMove #\X) (set! score (+ score 1)))
    ((char=? yourMove #\Y) (set! score (+ score 2)))
    ((char=? yourMove #\Z) (set! score (+ score 3)))
  )
  
  (cond
    ((or (and (char=? opponent #\A) (char=? yourMove #\Y)) 
         (and (char=? opponent #\B) (char=? yourMove #\Z)) 
         (and (char=? opponent #\C) (char=? yourMove #\X))) (set! score (+ score 6)))
    ((or (and (char=? opponent #\A) (char=? yourMove #\X)) 
         (and (char=? opponent #\B) (char=? yourMove #\Y)) 
         (and (char=? opponent #\C) (char=? yourMove #\Z))) (set! score (+ score 3)))
  )
)

(define (read-file)
  (let loop ((line (read-line totalScore)))
    (if (eof-object? line)
        (begin
          (close-input-port totalScore)
          score)
        (begin
          (calculate-score (string-ref line 0) (string-ref line 2))
          (loop (read-line totalScore)))
    )
  )
)

(define result (read-file))
(printf "~a\n" result)
