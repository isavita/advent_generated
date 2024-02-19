
#lang racket

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (look-and-say sequence iterations)
  (let loop ([sequence sequence] [i iterations])
    (if (zero? i)
        sequence
        (loop (next-sequence sequence) (- i 1)))))

(define (next-sequence sequence)
  (define result (open-output-string))
  (define (write-count-digit count digit)
    (fprintf result "~a~a" count digit))
  (define (loop [i 0])
    (cond
      [(= i (string-length sequence)) (get-output-string result)]
      [else
       (define count 1)
       (define digit (string-ref sequence i))
       (define j (+ i 1))
       (let loop2 ()
         (cond
           [(and (< j (string-length sequence)) (char=? (string-ref sequence j) digit))
            (set! count (+ count 1))
            (set! j (+ j 1))
            (loop2)]
           [else
            (write-count-digit count digit)
            (loop j)]))
       ]))
  (loop))

(define initial-sequence (read-input "input.txt"))
(define result (look-and-say initial-sequence 50))
(printf "~a\n" (string-length result))
