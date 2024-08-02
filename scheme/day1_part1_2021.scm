(define (count-increases depths)
  (define (helper prev count depths)
    (cond
      ((null? depths) count)
      ((> (car depths) prev)
       (helper (car depths) (+ count 1) (cdr depths)))
      (else
       (helper (car depths) count (cdr depths)))))
  (if (null? depths) 0
      (helper (car depths) 0 (cdr depths))))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (string->number line) lines))))))))

(define (main)
  (let ((depths (read-input "input.txt")))
    (display (count-increases depths))
    (newline)))

(main)