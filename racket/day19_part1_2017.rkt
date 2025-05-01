
#lang racket

(define (main)
  (define grid
    (with-input-from-file "input.txt"
      (lambda () (port->lines))))

  (define height (length grid))
  (define width (if (null? grid) 0 (string-length (car grid))))

  (define (get-cell x y)
    (if (and (>= x 0) (< x width) (>= y 0) (< y height))
        (string-ref (list-ref grid y) x)
        #\Space))

  (define start-x
    (let loop ([i 0])
      (if (>= i width)
          -1
          (if (char=? (get-cell i 0) #\|)
              i
              (loop (+ i 1))))))

  (let loop ([x start-x] [y 0] [dx 0] [dy 1] [letters '()])
    (define cell (get-cell x y))

    (cond
      [(char=? cell #\Space)
       (display (list->string (reverse letters)))]

      [(char-alphabetic? cell)
       (loop (+ x dx) (+ y dy) dx dy (cons cell letters))]

      [(char=? cell #\+)
       (if (= dx 0)
           (let ([left-cell (get-cell (- x 1) y)])
             (if (and (> x 0) (or (char=? left-cell #\-) (char-alphabetic? left-cell)))
                 (loop (- x 1) y -1 0 letters)
                 (loop (+ x 1) y 1 0 letters)))
           (let ([up-cell (get-cell x (- y 1))])
             (if (and (> y 0) (or (char=? up-cell #\|) (char-alphabetic? up-cell)))
                 (loop x (- y 1) 0 -1 letters)
                 (loop x (+ y 1) 0 1 letters))))]

      [else
       (loop (+ x dx) (+ y dy) dx dy letters)]))
)

(main)
