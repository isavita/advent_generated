
#lang racket

(define (count-ways design patterns)
  (let* ((n (string-length design))
         (dp (make-vector (+ n 1) 0)))
    (vector-set! dp 0 1)
    (for ((i (in-range 1 (+ n 1))))
      (for ((p patterns))
        (let ((lp (string-length p)))
          (when (>= i lp)
            (let ((suffix (substring design (- i lp) i)))
              (when (string=? suffix p)
                (vector-set! dp i (+ (vector-ref dp i) (vector-ref dp (- i lp))))))))))
    (vector-ref dp n)))

(module+ main
  (with-input-from-file "input.txt"
    (lambda ()
      (let* ((patterns-line (read-line))
             (available-patterns (map string-trim (string-split patterns-line ","))))
        (read-line) ; Skip the blank line
        (let loop ((total-ways 0))
          (let ((design-line (read-line)))
            (if (eof-object? design-line)
                (printf "~a\n" total-ways)
                (let* ((design (string-trim design-line))
                       (ways (count-ways design available-patterns)))
                  (loop (+ total-ways ways))))))))))
