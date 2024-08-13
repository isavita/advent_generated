#lang racket

(define (get-original-message messages)
  (define message-length (string-length (first messages)))
  (define count (for/list ([i message-length]) (make-hash)))
  
  (for ([message messages])
    (for ([j (in-range message-length)])
      (define char (string-ref message j))
      (hash-update! (list-ref count j) char add1 1)))
  
  (define (get-least-common-char char-count)
    (define min-char #\space)
    (define min-count +inf.0)
    (for ([char (hash-keys char-count)])
      (define cnt (hash-ref char-count char))
      (when (< cnt min-count)
        (set! min-count cnt)
        (set! min-char char)))
    min-char)
  
  (apply string (map get-least-common-char count)))

(define (main)
  (define input (file->lines "input.txt"))
  (define original-message (get-original-message input))
  (displayln original-message))

(main)