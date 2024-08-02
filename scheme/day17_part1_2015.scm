(define (read-containers filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((line (read-line)) (containers '()))
        (if (eof-object? line)
            (reverse containers)
            (loop (read-line) (cons (string->number line) containers)))))))

(define (count-combinations containers target)
  (define (count-ways remaining containers)
    (cond
      ((= remaining 0) 1)
      ((or (< remaining 0) (null? containers)) 0)
      (else (+ (count-ways remaining (cdr containers))
                (count-ways (- remaining (car containers)) (cdr containers))))))
  (count-ways target containers))

(define (main)
  (let ((containers (read-containers "input.txt")))
    (display (count-combinations containers 150))
    (newline)))

(main)