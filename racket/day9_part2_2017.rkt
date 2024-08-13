#lang racket

(define (process-stream input)
  (define score 0)
  (define depth 0)
  (define in-garbage #f)
  (define cancel-next #f)
  (define garbage-count 0)

  (for ([ch (in-string input)])
    (cond
      [(and cancel-next (set! cancel-next #f))]
      [(and in-garbage (char=? ch #\!)) (set! cancel-next #t)]
      [(and in-garbage (char=? ch #\>)) (set! in-garbage #f)]
      [(and in-garbage (not cancel-next)) (set! garbage-count (+ garbage-count 1))]
      [(char=? ch #\{) (set! depth (+ depth 1))]
      [(char=? ch #\}) (begin (set! score (+ score depth)) (set! depth (- depth 1)))]
      [(char=? ch #\<) (set! in-garbage #t)]))
  
  garbage-count)

(define (main)
  (define input (file->string "input.txt"))
  (define total-garbage (process-stream input))
  (display total-garbage))

(main)