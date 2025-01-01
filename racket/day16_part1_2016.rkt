
#lang racket

(define (reverse-and-invert s)
  (list->string
   (map (lambda (c) (if (char=? c #\0) #\1 #\0))
        (reverse (string->list s)))))

(define (dragon-curve initial-state disk-size)
  (let loop ([data initial-state])
    (if (>= (string-length data) disk-size)
        (substring data 0 disk-size)
        (loop (string-append data "0" (reverse-and-invert data))))))

(define (checksum data)
  (let loop ([current-data data])
    (if (odd? (string-length current-data))
        current-data
        (let ([next-data ""])
          (for ([i (in-range 0 (string-length current-data) 2)])
            (set! next-data (string-append next-data (if (char=? (string-ref current-data i) (string-ref current-data (+ i 1))) "1" "0"))))
          (loop next-data)))))

(define (solve input-string disk-size)
  (checksum (dragon-curve input-string disk-size)))

(define (main)
  (let* ([input-port (open-input-file "input.txt")]
         [input-string (read-line input-port)]
         [disk-size 272]
         [result (solve input-string disk-size)])
    (close-input-port input-port)
    (displayln result)))

(main)
