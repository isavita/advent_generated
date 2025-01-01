
#lang racket

(define mod (expt 2 24))
(define num-steps 2000)

(define (next-secret s)
  (define x (* s 64))
  (set! s (bitwise-xor s x))
  (set! s (bitwise-and s (- mod 1)))
  (set! x (quotient s 32))
  (set! s (bitwise-xor s x))
  (set! s (bitwise-and s (- mod 1)))
  (set! x (* s 2048))
  (set! s (bitwise-xor s x))
  (set! s (bitwise-and s (- mod 1)))
  s)

(define (encode-change4 c1 c2 c3 c4)
  (+ (+ c1 9) (* (+ c2 9) 19) (* (+ c3 9) 19 19) (* (+ c4 9) 19 19 19)))

(define (solve)
  (define initials
    (with-input-from-file "input.txt"
      (λ ()
        (for/list ([line (in-lines)] #:when (non-empty-string? line))
          (string->number line)))))

  (define buyers
    (for/list ([init-val initials])
      (define prices (build-vector (+ num-steps 1) (λ (x) 0)))
      (define s init-val)
      (for ([j (in-range (+ num-steps 1))])
        (vector-set! prices j (remainder s 10))
        (when (< j num-steps)
          (set! s (next-secret s))))
      (define changes (build-vector num-steps (λ (x) 0)))
      (for ([j (in-range num-steps)])
        (vector-set! changes j (- (vector-ref prices (+ j 1)) (vector-ref prices j))))
      (list prices changes)))

  (define pattern-count (* 19 19 19 19))
  (define global-sum (build-vector pattern-count (λ (x) 0)))

  (for ([b buyers])
    (define local-price (build-vector pattern-count (λ (x) -1)))
    (define prices (first b))
    (define changes (second b))
    (for ([i (in-range (- num-steps 3))])
      (define c1 (vector-ref changes i))
      (define c2 (vector-ref changes (+ i 1)))
      (define c3 (vector-ref changes (+ i 2)))
      (define c4 (vector-ref changes (+ i 3)))
      (when (and (<= -9 c1 9) (<= -9 c2 9) (<= -9 c3 9) (<= -9 c4 9))
        (define idx (encode-change4 c1 c2 c3 c4))
        (when (= (vector-ref local-price idx) -1)
          (vector-set! local-price idx (vector-ref prices (+ i 4))))))
    (for ([idx (in-range pattern-count)] [p (in-vector local-price)])
      (when (>= p 0)
        (vector-set! global-sum idx (+ (vector-ref global-sum idx) p)))))

  (apply max (vector->list global-sum)))

(printf "~a\n" (solve))
