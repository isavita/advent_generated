
#lang racket

(define (main)
  (define instructions (file->lines "input.txt"))
  (define code (get-bathroom-code instructions))
  (displayln code))

(define (file->lines filename)
  (define input (open-input-file filename))
  (define lines '())
  (let loop ()
    (define line (read-line input 'any))
    (if (eof-object? line)
        (begin (close-input-port input) (reverse lines))
        (begin (set! lines (cons line lines))
               (loop)))))

(define (get-bathroom-code instructions)
  (define keypad '((1 2 3) (4 5 6) (7 8 9)))
  (define x 1)
  (define y 1)
  (define code "")

  (for ([instruction instructions])
    (for ([move (string->list instruction)])
      (cond
        [(char=? move #\U) (when (> x 0) (set! x (sub1 x)))]
        [(char=? move #\D) (when (< x 2) (set! x (add1 x)))]
        [(char=? move #\L) (when (> y 0) (set! y (sub1 y)))]
        [(char=? move #\R) (when (< y 2) (set! y (add1 y)))]))
    (set! code (string-append code (number->string (list-ref (list-ref keypad x) y)))))

  code)

(main)
