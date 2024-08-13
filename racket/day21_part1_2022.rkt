#lang racket

(define (main)
  (define jobs (read-input "input.txt"))
  (define results (make-hash))
  (display (calculate "root" jobs results)))

(define (read-input filename)
  (define in (open-input-file filename))
  (define jobs (make-hash))
  (for ([line (in-lines in)])
    (define parts (string-split line ": "))
    (hash-set! jobs (first parts) (second parts)))
  (close-input-port in)
  jobs)

(define (calculate monkey jobs results)
  (if (hash-has-key? results monkey)
      (hash-ref results monkey)
      (let ([job (hash-ref jobs monkey)])
        (if (string->number job)
            (begin
              (hash-set! results monkey (string->number job))
              (string->number job))
            (let* ([parts (string-split job)]
                   [a (calculate (first parts) jobs results)]
                   [b (calculate (third parts) jobs results)]
                   [op (second parts)]
                   [result (cond
                             [(equal? op "+") (+ a b)]
                             [(equal? op "-") (- a b)]
                             [(equal? op "*") (* a b)]
                             [(equal? op "/") (/ a b)])])
              (hash-set! results monkey result)
              result)))))

(main)