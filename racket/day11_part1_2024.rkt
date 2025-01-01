
#lang racket

(define (solve)
  (define input-file "input.txt")
  (define stones (file->lines input-file))
  (define initial-stones (string-split (first stones) " "))

  (define (even-digits? s)
    (even? (string-length s)))

  (define (trim-leading-zeros s)
    (if (and (> (string-length s) 1) (equal? (substring s 0 1) "0"))
        (trim-leading-zeros (substring s 1))
        s))

  (define (process-stones stones)
    (let loop ([stones stones] [next '()])
      (cond
        [(null? stones) next]
        [else
         (let* ([s (car stones)]
                [rest (cdr stones)])
           (cond
             [(equal? s "0") (loop rest (append next (list "1")))]
             [(even-digits? s)
              (let* ([mid (quotient (string-length s) 2)]
                     [left (trim-leading-zeros (substring s 0 mid))]
                     [right (trim-leading-zeros (substring s mid))])
                (loop rest (append next (list (if (equal? left "") "0" left) (if (equal? right "") "0" right)))))]
             [else
              (let ([n (string->number s)])
                (loop rest (append next (list (number->string (* n 2024))))))]))])))

  (let loop ([i 0] [current-stones initial-stones])
    (if (= i 25)
        (length current-stones)
        (loop (+ i 1) (process-stones current-stones)))))

(displayln (solve))
