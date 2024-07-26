
#lang racket

(struct ip-range (start end))

(define (read-ip-ranges filename)
  (define lines (file->lines filename))
  (map (lambda (line)
         (let* ([parts (string-split line "-")]
                [start (string->number (first parts))]
                [end (string->number (second parts))])
           (ip-range start end)))
       lines))

(define (find-unblocked-ip ranges)
  (define (loop current-ip ranges)
    (cond
      [(null? ranges) current-ip]
      [else
       (define r (first ranges))
       (if (> (ip-range-start r) current-ip)
           current-ip
           (loop (max current-ip (+ (ip-range-end r) 1)) (rest ranges)))]))
  (loop 0 (sort ranges (lambda (a b) (< (ip-range-start a) (ip-range-start b))))))

(define (main)
  (define ip-ranges (read-ip-ranges "input.txt"))
  (define unblocked-ip (find-unblocked-ip ip-ranges))
  (displayln unblocked-ip))

(main)
