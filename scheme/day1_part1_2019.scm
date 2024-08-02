(define (calculate-fuel mass)
  (max 0 (- (floor (/ mass 3)) 2)))

(define (total-fuel requirements)
  (apply + (map calculate-fuel requirements)))

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((masses '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse masses)
              (loop (cons (string->number line) masses))))))))

(define (main)
  (let ((masses (read-input "input.txt")))
    (display (total-fuel masses))
    (newline)))

(main)