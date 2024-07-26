
#lang racket

(define (main)
  (define input (file->string "input.txt"))
  (define-values (depth target) (parse-input input))
  (define cave (make-cave-system depth target))
  (define risk-level (calculate-risk-level cave target))
  (displayln risk-level))

(define (parse-input data)
  (define lines (string-split data "\n"))
  (define depth (string->number (second (string-split (first lines) " "))))
  (define coords (second (string-split (second lines) " ")))
  (define parts (map string->number (string-split coords ",")))
  (values depth (list (first parts) (second parts))))

(define (make-cave-system depth target)
  (define width (first target))
  (define height (second target))
  (define cave (make-vector (add1 height)))
  (for ([y (in-range (add1 height))])
    (vector-set! cave y (make-vector (add1 width)))
    (for ([x (in-range (add1 width))])
      (define geologic-index
        (cond
          [(and (= x 0) (= y 0)) 0]
          [(and (= x (first target)) (= y (second target))) 0]
          [(= y 0) (* x 16807)]
          [(= x 0) (* y 48271)]
          [else (* (vector-ref (vector-ref cave y) (sub1 x))
                    (vector-ref (vector-ref cave (sub1 y)) x))]))
      (vector-set! (vector-ref cave y) x (modulo (+ geologic-index depth) 20183))))
  cave)

(define (calculate-risk-level cave target)
  (define risk-level 0)
  (for ([y (in-range (add1 (second target)))])
    (for ([x (in-range (add1 (first target)))])
      (set! risk-level (+ risk-level (modulo (vector-ref (vector-ref cave y) x) 3)))))
  risk-level)

(main)
