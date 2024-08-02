(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (sublist lst start end)
  (let loop ((lst lst) (index 0) (result '()))
    (cond ((null? lst) (reverse result))
          ((>= index start) (loop (cdr lst) (+ index 1) (cons (car lst) result)))
          (else (loop (cdr lst) (+ index 1) result)))))

(define (valid-sum? num preamble)
  (let loop ((nums preamble))
    (cond ((null? nums) #f)
          ((member (- num (car nums)) (cdr nums)) #t)
          (else (loop (cdr nums))))))

(define (find-invalid-number numbers preamble-length)
  (let loop ((index preamble-length))
    (if (>= index (length numbers))
        #f
        (let ((num (list-ref numbers index))
              (preamble (sublist numbers (- index preamble-length) index)))
          (if (valid-sum? num preamble)
              (loop (+ index 1))
              num)))))

(define (main)
  (let ((numbers (read-input "input.txt")))
    (display (find-invalid-number numbers 25))))

(main)