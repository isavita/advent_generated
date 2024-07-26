
#lang racket

(define (calculate-encoded-length s)
  (define encoded (string-append "\"" 
                                  (apply string-append 
                                         (map (lambda (ch)
                                                (if (or (char=? ch #\\) (char=? ch #\")) 
                                                    (string-append "\\" (string ch)) 
                                                    (string ch)))
                                              (string->list s)))
                                  "\""))
  (string-length encoded))

(define (calculate-total-diff filename)
  (define in (open-input-file filename))
  (define total-diff (for/sum ([line (in-lines in)])
                       (let ([original-length (string-length line)]
                             [encoded-length (calculate-encoded-length line)])
                         (- encoded-length original-length))))
  (close-input-port in)
  total-diff)

(define result (calculate-total-diff "input.txt"))
(displayln result)
