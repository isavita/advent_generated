
#lang racket

(define (calculate-memory-length s)
  (let loop ([i 1] [length 0] [in-escape #f] [hex-count 0])
    (cond
      [(= i (- (string-length s) 1)) length]
      [(> hex-count 0) (loop (+ i 1) length in-escape (- hex-count 1))]
      [in-escape
       (loop (+ i 1) (+ length 1) #f (if (char=? (string-ref s i) #\x) 2 0))]
      [(char=? (string-ref s i) #\\) (loop (+ i 1) length #t hex-count)]
      [else (loop (+ i 1) (+ length 1) in-escape hex-count)])))

(define (solve)
  (let ([total-diff 0])
    (with-input-from-file "input.txt"
      (lambda ()
        (let loop ()
          (let ([line (read-line)])
            (unless (eof-object? line)
              (let* ([code-length (string-length line)]
                     [memory-length (calculate-memory-length line)])
                (set! total-diff (+ total-diff (- code-length memory-length))))
              (loop))))))
    total-diff))

(display (solve))
