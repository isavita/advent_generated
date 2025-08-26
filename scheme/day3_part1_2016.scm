
(define (valid? a b c)
  (and (> (+ a b) c)
       (> (+ a c) b)
       (> (+ b c) a)))

(define (main)
  (let ((count 0))
    (call-with-input-file "input.txt"
      (lambda (port)
        (let loop ()
          (let ((a (read port))
                (b (read port))
                (c (read port)))
            (if (eof-object? c)
                (display count)
                (begin
                  (when (valid? a b c) (set! count (+ count 1)))
                  (loop))))))))
  (newline))

(main)
