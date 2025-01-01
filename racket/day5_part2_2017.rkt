
#lang racket

(define (solve filename part)
  (define instructions
    (with-input-from-file filename
      (lambda ()
        (let loop ((lines (read-line)) (acc '()))
          (if (eof-object? lines)
              (reverse acc)
              (loop (read-line) (cons (string->number lines) acc)))))))
  
  (define (step instructions current-index steps)
    (if (or (< current-index 0) (>= current-index (length instructions)))
        steps
        (let* ((offset (list-ref instructions current-index))
               (new-offset (if (and (= part 2) (>= offset 3)) (- offset 1) (+ offset 1)))
               (new-instructions (list-update instructions current-index new-offset))
               (next-index (+ current-index offset)))
          (step new-instructions next-index (+ steps 1)))))
  
  (step instructions 0 0))

(define (list-update lst index new-value)
  (if (zero? index)
      (cons new-value (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- index 1) new-value))))

(displayln (solve "input.txt" 1))
(displayln (solve "input.txt" 2))
