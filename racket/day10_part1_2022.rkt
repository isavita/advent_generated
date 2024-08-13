#lang racket

(define (read-all path)
  (with-input-from-file path (lambda () (port->string (current-input-port)))))

(define (main)
  (define x (list 1))
  (for-each 
   (lambda (line)
     (cond 
       [(string=? line "noop") (set! x (append x (list (last x))))]
       [else 
        (define n (string->number (cadr (string-split line))))
        (set! x (append x (list (last x) (+ (last x) n))))]))
   (string-split (read-all "input.txt") "\n"))
  
  (define sum 
    (for/sum ([i (in-range (length x))] #:when (zero? (modulo (- i 19) 40)))
      (* (+ i 1) (list-ref x i))))
  
  (displayln sum))

(main)