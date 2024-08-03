(define (is-prime n)
  (define (check-divisor i)
    (if (> (* i i) n)
        #t
        (if (= (modulo n i) 0)
            #f
            (check-divisor (+ i 1)))))
  (if (< n 2) #f (check-divisor 2)))

(define (count-non-primes)
  (define b (+ (* 57 100) 100000))
  (define c (+ b 17000))
  (define h 0)
  (do ((x b (+ x 17)))
      ((> x c) h)
    (if (not (is-prime x))
        (set! h (+ h 1)))))

(display (count-non-primes))