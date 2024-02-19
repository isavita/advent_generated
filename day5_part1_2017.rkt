
#lang racket

(define (read-input)
  (with-input-from-file "input.txt"
    (lambda ()
      (let loop ([result '()])
        (let ([line (read-line)])
          (if (eof-object? line)
              (reverse result)
              (loop (cons (string->number line) result))))))))

(define (maze-exit-steps jumps)
  (let loop ([steps 0]
             [index 0]
             [jumps jumps])
    (if (>= index (length jumps))
        steps
        (let ([next-index (+ index (list-ref jumps index))])
          (loop (+ steps 1)
                next-index
                (update-list jumps index add1))))))

(define (update-list lst index func)
  (if (null? lst)
      '()
      (if (= index 0)
          (cons (func (car lst)) (cdr lst))
          (cons (car lst) (update-list (cdr lst) (- index 1) func)))))

(define input (read-input))
(display (maze-exit-steps input))
