
#lang racket

(define (has-abba? s)
  (and (= (string-length s) 4)
       (not (char=? (string-ref s 0) (string-ref s 1)))
       (char=? (string-ref s 0) (string-ref s 3))
       (char=? (string-ref s 1) (string-ref s 2))))

(define (supports-tls? ip)
  (let ([len (string-length ip)])
    (let loop ([i 0]
               [in-hypernet? #f]
               [found-abba-outside? #f]
               [found-abba-inside? #f])
      (cond
        [(= i len) (and found-abba-outside? (not found-abba-inside?))]

        [(char=? (string-ref ip i) #\[)
         (loop (add1 i) #t found-abba-outside? found-abba-inside?)]

        [(char=? (string-ref ip i) #\])
         (loop (add1 i) #f found-abba-outside? found-abba-inside?)]

        [(>= (- len i) 4)
         (let ([segment (substring ip i (+ i 4))])
           (if (has-abba? segment)
               (if in-hypernet?
                   (loop (add1 i) in-hypernet? found-abba-outside? #t)
                   (loop (add1 i) in-hypernet? #t found-abba-inside?))
               (loop (add1 i) in-hypernet? found-abba-outside? found-abba-inside?)))]

        [else
         (loop (add1 i) in-hypernet? found-abba-outside? found-abba-inside?)]))))

(define (main)
  (let ([file-path "input.txt"])
    (with-input-from-file file-path
      (lambda ()
        (let ([lines (port->lines (current-input-port))])
          (let ([count 0])
            (for ([ip lines])
              (when (supports-tls? (string-trim ip))
                (set! count (add1 count))))
            (print count)))))))

(main)
