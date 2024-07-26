
#lang racket

(define (main)
  (define s (read-all "input.txt"))
  (displayln (first-n-unique s 14)))

(define (first-n-unique s n)
  (for/fold ([i n]) ([j (in-range n (string-length s))])
    (if (and (= i n) (unique? (substring s (- j n) j)))
        j
        i)))

(define (unique? str)
  (define chars (string->list str))
  (define set (make-hash))
  (for ([c chars])
    (hash-set! set c #t))
  (= (length chars) (hash-count set)))

(define (read-all path)
  (string-trim (file->string path)))

(main)
