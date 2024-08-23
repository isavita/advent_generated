(define (sum-of-divisors n)
  (define (divisors d)
    (if (> d (sqrt n))
        '()
        (if (= (modulo n d) 0)
            (if (= d (/ n d))
                (cons d (divisors (+ d 1)))
                (cons d (cons (/ n d) (divisors (+ d 1)))))
            (divisors (+ d 1)))))
  (apply + (divisors 1)))

(define (find-house target)
  (define (search-house house)
    (if (>= (* 10 (sum-of-divisors house)) target)
        house
        (search-house (+ house 1))))
  (search-house 1))

(define target
  (with-input-from-file "input.txt"
    (lambda () (read))))

(define result (find-house target))
(display result)