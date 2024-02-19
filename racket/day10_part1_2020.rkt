
#lang racket

(define (read-input)
  (let ((port (open-input-file "input.txt")))
    (let loop ((result '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin
              (close-input-port port)
              result)
            (loop (cons (string->number line) result)))))))

(define input (sort (read-input) <))

(define (count-differences lst)
  (let loop ((lst lst) (prev 0) (diff1 0) (diff3 1))
    (cond
      ((null? lst) (* diff1 diff3))
      ((= (car lst) (+ prev 1)) (loop (cdr lst) (car lst) (+ diff1 1) diff3))
      ((= (car lst) (+ prev 3)) (loop (cdr lst) (car lst) diff1 (+ diff3 1))))))

(display (count-differences input))
