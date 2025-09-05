(define (from-snafu s)
  (let loop ((i 0) (n 0))
    (if (= i (string-length s))
        n
        (let ((c (string-ref s i)))
          (let ((n2 (* n 5)))
            (cond ((char=? c #\=) (loop (+ i 1) (- n2 2)))
                  ((char=? c #\-) (loop (+ i 1) (- n2 1)))
                  (else (loop (+ i 1) (+ n2 (- (char->integer c) (char->integer #\0)))))))))))

(define (print-snafu-n n)
  (if (= n 0)
      '()
      (let* ((r (modulo n 5))
             (n2 (if (or (= r 3) (= r 4))
                     (quotient (+ n 5) 5)
                     (quotient n 5)))
             (ch (cond ((= r 3) #\=)
                       ((= r 4) #\-)
                       (else (integer->char (+ (char->integer #\0) r))))))
        (begin
          (print-snafu-n n2)
          (display ch)))))

(define (print-snafu n)
  (if (= n 0)
      (begin (display "0") (newline))
      (begin (print-snafu-n n) (newline))))

(define (main)
  (call-with-input-file "input.txt"
    (lambda (in)
      (let loop ((sum 0))
        (let ((line (read-line in)))
          (if (eof-object? line)
              (print-snafu sum)
              (loop (+ sum (from-snafu line)))))))))

(main)