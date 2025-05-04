
#lang racket

(require racket/file)
(require racket/string)

(define (main)
  (define input-line (string-trim (file->string "input.txt")))
  (define offset (string->number (substring input-line 0 7)))
  (define input-length (string-length input-line))
  (define repeat-factor 10000)
  (define total-length (* input-length repeat-factor))
  (define signal (make-vector total-length))

  (for ([i (in-range total-length)])
    (vector-set! signal i (string->number (string (string-ref input-line (modulo i input-length))))))

  (for ([phase (in-range 100)])
    (let ([total 0])
      (for ([i (in-range (sub1 total-length) (sub1 offset) -1)])
        (set! total (+ total (vector-ref signal i)))
        (vector-set! signal i (modulo total 10)))))

  (define result-string
    (string-append*
     (for/list ([i (in-range offset (+ offset 8))])
       (number->string (vector-ref signal i)))))

  (println result-string))

(main)
