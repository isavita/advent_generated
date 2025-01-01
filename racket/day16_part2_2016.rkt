
#lang racket

(define disk-length 35651584)

(define (read-initial-state filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (generate-data initial-state length)
  (let loop ([data initial-state])
    (if (>= (string-length data) length)
        (substring data 0 length)
        (let* ([b (list->string (map (lambda (x) (if (char=? x #\0) #\1 #\0)) (reverse (string->list data))))])
          (loop (string-append data "0" b))))))

(define (calculate-checksum data)
  (let loop ([data data])
    (if (odd? (string-length data))
        data
        (let* ([new-data (list->string (for/list ([i (in-range 0 (string-length data) 2)])
                                          (if (char=? (string-ref data i) (string-ref data (+ i 1)))
                                              #\1
                                              #\0)))])
          (loop new-data)))))

(define initial-state (read-initial-state "input.txt"))
(define data (generate-data initial-state disk-length))
(define checksum (calculate-checksum data))
(displayln checksum)
