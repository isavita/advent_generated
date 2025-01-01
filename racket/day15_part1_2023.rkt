
#lang racket

(define (hash-string str)
  (let loop ([res 0] [i 0])
    (if (= i (string-length str))
        res
        (loop (remainder (* (+ res (char->integer (string-ref str i))) 17) 256) (+ i 1)))))

(define (solve input)
  (let* ([line (first input)]
         [steps (string-split line ",")])
    (apply + (map hash-string steps))))

(define (read-file file-name)
  (call-with-input-file file-name
    (lambda (in)
      (string-split (string-trim (port->string in)) "\n"))))

(let ([input (read-file "input.txt")])
  (displayln (solve input)))
