
#lang racket

(define (calculate-points winning-numbers your-numbers)
  (let ([points (foldl (lambda (num acc)
                         (if (member num winning-numbers)
                             (if (= acc 0) 1 (* acc 2))
                             acc))
                       0
                       your-numbers)])
    points))

(define (read-input filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ([total-points 0])
        (let ([line (read-line in 'any)])
          (if (eof-object? line)
              total-points
              (let* ([parts (string-split line " | ")]
                     [winning-numbers (map string->number (string-split (first parts)))]
                     [your-numbers (map string->number (string-split (second parts)))])
                (loop (+ total-points (calculate-points winning-numbers your-numbers))))))))))

(printf "~a\n" (read-input "input.txt"))
