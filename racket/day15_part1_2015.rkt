
#lang racket

(define (read-ingredients filename)
  (with-input-from-file filename
    (lambda ()
      (define ingredients '())
      (let loop ()
        (define line (read-line))
        (unless (eof-object? line)
          (define parts (string-split line #px"[:, ]+" #:trim? #t))
          (when (>= (length parts) 10)
            (define capacity (string->number (list-ref parts 2)))
            (define durability (string->number (list-ref parts 4)))
            (define flavor (string->number (list-ref parts 6)))
            (define texture (string->number (list-ref parts 8)))
            (set! ingredients (cons (list capacity durability flavor texture) ingredients)))
          (loop)))
      (reverse ingredients))))

(define (score ingredients teaspoons-vec)
  (define n (length ingredients))
  (define capacity-sum 0)
  (define durability-sum 0)
  (define flavor-sum 0)
  (define texture-sum 0)

  (for ([i (in-range n)])
    (define props (list-ref ingredients i))
    (define teaspoons (vector-ref teaspoons-vec i))
    (set! capacity-sum (+ capacity-sum (* (list-ref props 0) teaspoons)))
    (set! durability-sum (+ durability-sum (* (list-ref props 1) teaspoons)))
    (set! flavor-sum   (+ flavor-sum   (* (list-ref props 2) teaspoons)))
    (set! texture-sum  (+ texture-sum  (* (list-ref props 3) teaspoons))))

  (define final-capacity (max 0 capacity-sum))
  (define final-durability (max 0 durability-sum))
  (define final-flavor (max 0 flavor-sum))
  (define final-texture (max 0 texture-sum))

  (* final-capacity final-durability final-flavor final-texture))

(define (find-max-score ingredients total-teaspoons)
  (define n (length ingredients))
  (define teaspoons-vec (make-vector n 0))
  (define max-score 0)

  (letrec ((calculate (lambda (index remaining)
                        (if (= index (- n 1))
                            (begin
                              (vector-set! teaspoons-vec index remaining)
                              (set! max-score (max max-score (score ingredients teaspoons-vec))))
                            (for ([i (in-range (add1 remaining))])
                              (vector-set! teaspoons-vec index i)
                              (calculate (add1 index) (- remaining i)))))))
    (calculate 0 total-teaspoons))

  max-score)

(define filename "input.txt")
(define ingredients (read-ingredients filename))
(define total-teaspoons 100)
(define result (find-max-score ingredients total-teaspoons))

(print result)
