
#lang racket

(define (find-most-common chars)
  (define freq-ht (make-hash))
  (for ([char chars])
    (hash-update! freq-ht char add1 0))
  (define max-char #f)
  (define max-count -1)
  (hash-for-each freq-ht
                 (lambda (char count)
                   (when (> count max-count)
                     (set! max-count count)
                     (set! max-char char))))
  max-char)

(define (main)
  (define lines (with-input-from-file "input.txt" port->lines))
  (when (empty? lines)
    (error "input.txt is empty"))
  (define message-length (string-length (first lines)))
  (define message-chars
    (for/list ([i (in-range message-length)])
      (define column-chars (map (lambda (line) (string-ref line i)) lines))
      (find-most-common column-chars)))
  (printf "~a\n" (list->string message-chars)))

(main)
