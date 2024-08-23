(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (count-combinations containers target)
  (define (count-ways containers target current-count)
    (cond
      ((= target 0) (list current-count))
      ((or (< target 0) (null? containers)) '())
      (else
       (append
        (count-ways (cdr containers) target current-count)
        (count-ways (cdr containers) (- target (car containers)) (+ current-count 1))))))
  (count-ways containers target 0))

(define (min-container-ways combinations)
  (let ((min-containers (apply min combinations)))
    (length (filter (lambda (x) (= x min-containers)) combinations))))

(define (main)
  (let* ((containers (read-input "input.txt"))
         (combinations (count-combinations containers 150)))
    (display (min-container-ways combinations))
    (newline)))

(main)