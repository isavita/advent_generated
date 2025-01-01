
#lang racket

(define width 101)
(define height 103)

(define (solve)
  (define robots
    (for/list ([line (in-lines (open-input-file "input.txt"))])
      (match (string-split line)
        [(list (app (lambda (s) (substring s 2)) p-str) (app (lambda (s) (substring s 2)) v-str))
         (append (map string->number (string-split p-str ","))
                 (map string->number (string-split v-str ",")))])))

  (for ([i (in-range 100)])
    (set! robots
          (for/list ([r robots])
            (define x (modulo (+ (list-ref r 0) (list-ref r 2)) width))
            (define y (modulo (+ (list-ref r 1) (list-ref r 3)) height))
            (list (if (< x 0) (+ x width) x)
                  (if (< y 0) (+ y height) y)
                  (list-ref r 2)
                  (list-ref r 3)))))

  (define-values (q1 q2 q3 q4)
    (for/fold ([q1 0] [q2 0] [q3 0] [q4 0]) ([r robots])
      (define x (list-ref r 0))
      (define y (list-ref r 1))
      (cond
        [(= x 50) (values q1 q2 q3 q4)]
        [(= y 51) (values q1 q2 q3 q4)]
        [(and (< x 50) (< y 51)) (values (+ q1 1) q2 q3 q4)]
        [(and (> x 50) (< y 51)) (values q1 (+ q2 1) q3 q4)]
        [(and (< x 50) (> y 51)) (values q1 q2 (+ q3 1) q4)]
        [(and (> x 50) (> y 51)) (values q1 q2 q3 (+ q4 1))]
        [else (values q1 q2 q3 q4)])))

  (* q1 q2 q3 q4))

(printf "~a\n" (solve))
