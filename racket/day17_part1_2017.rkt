
#lang racket

(define (read-input)
  (with-input-from-file "input.txt"
    (lambda ()
      (string->number (read-line)))))

(define (insert-after lst pos val)
  (append (take lst (add1 pos)) (list val) (drop lst (add1 pos))))

(define (find-next lst val)
  (let loop ((lst lst) (found #f))
    (cond
      [(null? lst) (error "Value not found")]
      [(and found (not (null? lst))) (car lst)]
      [(equal? (car lst) val) (loop (cdr lst) #t)]
      [else (loop (cdr lst) found)])))

(define (main)
  (define steps (read-input))
  (define buffer '(0))
  (define current-pos 0)

  (for ([i (in-range 1 2018)])
    (set! current-pos (modulo (+ current-pos steps) (length buffer)))
    (set! buffer (insert-after buffer current-pos i))
    (set! current-pos (add1 current-pos)))

  (define result (find-next buffer 2017))
  (displayln result))

(main)
