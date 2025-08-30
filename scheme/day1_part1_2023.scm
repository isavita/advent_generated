#!/usr/bin/env scheme

(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (line-value line)
  (let loop ((i 0) (first #f) (last #f))
    (if (>= i (string-length line))
        (if (and first last)
            (+ (* first 10) last)
            0)
        (let ((c (string-ref line i)))
          (if (char-numeric? c)
              (let ((d (- (char->integer c) (char->integer #\0))))
                (if first
                    (loop (+ i 1) first d)
                    (loop (+ i 1) d d)))
              (loop (+ i 1) first last))))))

(define (main)
  (let* ((lines (read-lines "input.txt"))
         (values (map line-value lines))
         (total (apply + values)))
    (display total)
    (newline)))

(main)
