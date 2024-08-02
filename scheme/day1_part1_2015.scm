#!/usr/bin/env scheme

(define (read-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((result '())
                 (char (read-char port)))
        (if (eof-object? char)
            (reverse result)
            (loop (cons char result) (read-char port)))))))

(define (calculate-floor instructions)
  (let loop ((instructions instructions)
             (floor 0))
    (if (null? instructions)
        floor
        (loop (cdr instructions)
              (case (car instructions)
                ((#\() (+ floor 1))
                ((#\)) (- floor 1)))))))

(define (main)
  (let* ((instructions (read-file "input.txt"))
         (final-floor (calculate-floor instructions)))
    (display final-floor)
    (newline)))

(main)