(define (look-and-say s)
  (define (next-term seq)
    (if (null? seq) ""
        (let loop ((current (car seq)) (count 1) (rest (cdr seq)) (result ""))
          (cond ((null? rest) (string-append result (number->string count) (string current)))
                ((equal? current (car rest)) (loop current (+ count 1) (cdr rest) result))
                (else (loop (car rest) 1 (cdr rest) (string-append result (number->string count) (string current))))))))
  (next-term (string->list s)))

(define (process-sequence input iterations)
  (define (iter seq count)
    (if (= count 0) seq
        (iter (look-and-say seq) (- count 1))))
  (iter input iterations))

(define (main)
  (let ((input (with-input-from-file "input.txt"
                 (lambda () (read-line)))))
    (define final-sequence (process-sequence input 40))
    (display (string-length final-sequence))
    (newline)))

(main)