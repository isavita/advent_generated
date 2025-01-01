
#lang racket

(define (read-input filename)
  (call-with-input-file filename
    (lambda (in)
      (let loop ([rules '()] [updates '()] [is-update-section #f])
        (let ([line (read-line in 'any)])
          (cond
            [(eof-object? line) (values rules updates)]
            [(string=? line "") (loop rules updates #t)]
            [else
             (if is-update-section
                 (let ([update (map string->number (string-split line ","))])
                   (if (not (null? update))
                       (loop rules (append updates (list update)) #t)
                       (loop rules updates #t)))
                 (let* ([parts (string-split line "|")]
                        [x (string->number (string-trim (list-ref parts 0)))]
                        [y (string->number (string-trim (list-ref parts 1)))])
                   (if (and (number? x) (number? y))
                       (loop (append rules (list (list x y))) updates #f)
                       (loop rules updates #f))))]))))))

(define (is-correctly-ordered? update rules)
  (let ([position (make-hash)])
    (for ([page update] [idx (in-naturals)])
      (hash-set! position page idx))
    (for/and ([rule rules])
      (let* ([x (list-ref rule 0)]
             [y (list-ref rule 1)]
             [pos-x (hash-ref position x #f)]
             [pos-y (hash-ref position y #f)])
        (or (not (and pos-x pos-y))
            (< pos-x pos-y))))))

(define (main)
  (let-values ([(rules updates) (read-input "input.txt")])
    (let loop ([updates updates] [sum 0])
      (cond
        [(null? updates) (printf "~a\n" sum)]
        [(is-correctly-ordered? (car updates) rules)
         (loop (cdr updates) (+ sum (list-ref (car updates) (quotient (length (car updates)) 2))))]
        [else (loop (cdr updates) sum)]))))

(main)
