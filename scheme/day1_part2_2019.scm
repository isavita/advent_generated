(define (calculate-fuel mass)
  (max 0 (- (floor (/ mass 3)) 2)))

(define (total-fuel mass)
  (let loop ((current-fuel (calculate-fuel mass))
             (total 0))
    (if (<= current-fuel 0)
        total
        (loop (calculate-fuel current-fuel) (+ total current-fuel)))))

(define (sum-fuel-requirements filename)
  (with-input-from-file filename
    (lambda ()
      (define total 0)
      (let loop ()
        (let ((line (read-line)))
          (if (eof-object? line)
              total
              (begin
                (set! total (+ total (total-fuel (string->number line))))
                (loop))))))))

(display (sum-fuel-requirements "input.txt"))