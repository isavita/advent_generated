
(define (sum-numbers str)
  (let loop ((i 0) (sum 0) (num 0) (neg 0))
    (if (= i (string-length str))
        (+ sum (if (= neg 1) (- num) num))
        (let ((c (string-ref str i)))
          (cond
            ((char=? c #\-) (loop (+ i 1) sum num 1))
            ((char<=? #\0 c #\9)
             (loop (+ i 1) sum (+ (* num 10) (- (char->integer c) 48)) neg))
            (else
             (loop (+ i 1)
                   (+ sum (if (= neg 1) (- num) num))
                   0 0)))))))

(define (main)
  (call-with-input-file "input.txt"
    (lambda (port)
      (let ((content (read-line port)))
        (display (sum-numbers content))
        (newline)))))

(main)
