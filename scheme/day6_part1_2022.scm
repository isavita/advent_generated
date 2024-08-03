(define (remove-duplicates lst)
  (define (helper lst seen)
    (cond
      ((null? lst) '())
      ((member (car lst) seen) (helper (cdr lst) seen))
      (else (cons (car lst) (helper (cdr lst) (cons (car lst) seen))))))
  (helper lst '()))

(define (unique? lst)
  (= (length lst) (length (remove-duplicates lst))))

(define (find-marker datastream)
  (define (loop index)
    (if (>= index (- (string-length datastream) 3))
        #f
        (let ((chars (substring datastream index (+ index 4))))
          (if (unique? (string->list chars))
              (+ index 4)
              (loop (+ index 1))))))
  (loop 0))

(define (main)
  (define input (with-input-from-file "input.txt"
                      (lambda () (read-line))))
  (display (find-marker input))
  (newline))

(main)