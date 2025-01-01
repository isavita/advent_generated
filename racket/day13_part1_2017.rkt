
#lang racket

(define (solve)
  (define firewall (make-hash))
  (define max-depth 0)
  (call-with-input-file "input.txt"
    (lambda (in)
      (for ([line (in-lines in)])
        (define fields (string-split line ": "))
        (define depth (string->number (first fields)))
        (define rng (string->number (second fields)))
        (hash-set! firewall depth (list rng 0 1))
        (set! max-depth (max max-depth depth)))))

  (define (move-scanner! scanner)
    (let* ([rng (first scanner)]
           [pos (second scanner)]
           [dir (third scanner)]
           [new-dir (cond [(= pos 0) 1]
                          [(= pos (- rng 1)) -1]
                          [else dir])]
           [new-pos (+ pos new-dir)])
      (list rng new-pos new-dir)))

  (define (calculate-severity)
    (let loop ([depth 0] [severity 0])
      (cond
        [(= depth (+ max-depth 1)) severity]
        [else
         (define scanner (hash-ref firewall depth #f))
         (define new-severity
           (if (and scanner (= (second scanner) 0))
               (+ severity (* depth (first scanner)))
               severity))
         (for ([(k v) firewall])
           (hash-set! firewall k (move-scanner! v)))
         (loop (+ depth 1) new-severity)])))
  
  (printf "~a\n" (calculate-severity)))

(solve)
